(* Translation from Java into simple-Java *)
open Java_syntax
open Simple_java_syntax


(* Many features of Java are not supported *)
(* When an unsupported feature is encoutered an exception is raised *)
exception Non_supported of string
let non_supported s =
  raise (Non_supported (Printf.sprintf "%s unsupported" s))
let check_support s b =
  if not b then non_supported s


(* Management of environments *)
module StringMap = Map.Make( String )

(* Mapping: class -> name -> index *)
type 'a global_env = 'a StringMap.t StringMap.t
let add_global_env
    (table: 'a global_env) (cn: string) (vn: string) (x: 'a) =
  let class_xs =
    try StringMap.find cn table
    with Not_found -> StringMap.empty in
  assert (not (StringMap.mem vn class_xs));
  StringMap.add cn (StringMap.add vn x class_xs) table
  
(* Find a name in a global environment *)
let find_global_env
    (table: 'a global_env) ((cn, vn): string * string): 'a =
  try StringMap.find vn (StringMap.find cn table)
  with
  | Not_found -> failwith (Printf.sprintf "variable %s.%s not found" cn vn)

type local_env = (s_var StringMap.t) list

type env =
    { e_gvars: s_var global_env ;
      e_class: string ;
      e_locs:  local_env }
let empty_env =
  { e_gvars = StringMap.empty ;
    e_class = "" ;
    e_locs  = [ ] ; }
let find_pointed (env: env) (cn, vn) =
  find_global_env env.e_gvars (cn, vn)
let find_non_pointed (env: env) (vn: string) =
  let rec aux = function
    | [ ] -> find_pointed env (env.e_class, vn)
    | loc :: rem ->
        try StringMap.find vn loc
        with Not_found -> aux rem in
  aux env.e_locs
let add_local_var (v: s_var) (env: env) =
  match env.e_locs with
  | [ ] -> failwith "stack issue"
  | loc0 :: others ->
      { env with
        e_locs = (StringMap.add v.s_var_name v loc0) :: others }


(* Translation of variables, and management of indexes *)

(* Types: only int and bool are supported *)
let tr_type = function
  | Boolean -> St_bool
  | Integer Int -> St_int
  | Void -> St_void
  | _ -> non_supported "type"
(* Management of indexes *)
let cur_id = ref 0
let new_id ( ) = let i = !cur_id in incr cur_id; i
(* translation of variables *)
let tr_variable (v: variable) =
  { s_var_name     = fst v.var_name ;
    s_var_extent   = snd v.var_name ;
    s_var_type     = tr_type v.var_type ;
    s_var_uniqueId = new_id () ; }


(* Generation of the global environment *)
let generate_env =
  let gen_env_class_decl
      (env: env) (cd: class_decl): env =
    let cn = fst cd.class_name in
    List.fold_left
      (fun acc_env d ->
        match d with
        | Class_member (Field_decl f) ->
            let var = tr_variable f in
            { acc_env with
              e_gvars = add_global_env acc_env.e_gvars cn
		var.s_var_name var }
        | _ -> acc_env
      ) env cd.class_body in
  let gen_env_java_prog (jp: java_prog) =
    List.fold_left
      (fun acc_env d ->
        match d with
        | Class_declaration cd -> gen_env_class_decl acc_env cd
        | _ -> acc_env
      ) empty_env jp.type_decls in
  gen_env_java_prog


(* Translation of Java into Simple_java *)
(* Unary operators: only boolean negation supported *)
let tr_unary_op = function
  | Not -> Su_neg
  | _ -> non_supported "unary operator"
(* Binary operators *)
let tr_binary_op = function
  | Or -> Sb_or
  | And | Xor -> non_supported "boolean operator"
  | Bitwise_and | Bitwise_or | Lshift | Rshift | Urshift ->
      non_supported "bitwise operator"
  | Equal -> non_supported "equal"
  | Lesser_than -> Sb_lt
  | Greater_than | Lesser_equal | Greater_equal ->
      non_supported "test"
  | Add -> Sb_add
  | Sub -> Sb_sub
  | Mult -> Sb_mul
  | Div -> Sb_div
  | Modulo -> non_supported "modulo"
  | Array_access -> non_supported "array access"

(* Variables: names are translated into a dummy variable *)
(* (which should be resolved later) *)
let tr_variable_name (env: env) (n: name) =
  match n with
  | Simple_name (vn, _) -> 
      find_non_pointed env vn
  | Qualified_name (Simple_name (cn, _), vn) ->
      find_pointed env (cn, vn)
  | _ -> non_supported "nested accesses"

let tr_formal_parameter (p: formal_parameter) =
  check_support "final parameters" (not p.param_is_final);
  check_support "ellipsis parameters" (not p.param_is_ellipsis);
  { s_var_name     = fst p.param_name ;
    s_var_extent   = snd p.param_name ;
    s_var_type     = tr_type p.param_type ;
    s_var_uniqueId = new_id () ; }

let rec tr_expression (env: env) (e: expression) =
  match e with
  | Assignment _ -> non_supported "assignment inside expressions"
  | Conditional _ -> non_supported "( e ? e : e )"
  | Binary (b, e0, e1) ->
      Se_binary (tr_binary_op b, tr_expr_e env e0, tr_expr_e env e1)
  | Unary (u, e0) ->
      Se_unary (tr_unary_op u, tr_expr_e env e0)
  | Variable name ->
      Se_var (tr_variable_name env name)
  | Integer_constant i ->
      Se_const (Sc_int i)
  | Float_constant _ -> non_supported "floating-point"
  | Bool_constant b ->
      Se_const (Sc_bool b)
  | Method_call (m, args) ->
      let e_args = List.map (tr_expr_e env) args in
      let funcall cl n =
        Se_function_call { s_fun_call_class  = cl ;
                           s_fun_call_name   = n ;
                           s_fun_call_params = e_args } in
      begin
        match m with
        | Named_method (Simple_name n) -> funcall env.e_class (fst n)
        | Named_method (Qualified_name (Simple_name n, f)) ->
            if String.compare (fst n) "Support" = 0
                && String.compare f "random" = 0 then
              begin
                match args with
                | [ Integer_constant i0, _; Integer_constant i1, _ ] ->
                    Se_random (i0, i1)
                | _ -> failwith "Support.random: arguments"
              end
            else funcall (fst n) f
        | _ -> non_supported "method call"
      end
  | Null -> non_supported "null"
  | Char_constant _ -> non_supported "char"
  | String_constant _ -> non_supported "string"
  | This -> non_supported "this"
  | New _ -> non_supported "new"
  | Field_access _ -> non_supported "field access"
  | New_array _ -> non_supported "new array"
  | Instanceof _ -> non_supported "instance of"

and tr_expr_e (env: env) (expr, ext) =
  tr_expression env expr, ext

and tr_initializer_opt (env: env) = function
  | None -> None
  | Some (Expr_init e) -> Some (tr_expr_e env e)
  | Some (Array_init _) -> non_supported "array initializers"

let tr_var_decl (isglobal: bool) (env: env) (v: variable): s_var_decl =
  let var =
    if isglobal then
      begin
        check_support "non-static fields" (List.mem Static v.var_modifiers);
        check_support "class modifiers" (List.length v.var_modifiers = 1);
        find_global_env env.e_gvars (env.e_class, fst v.var_name)
      end
    else
      begin
        check_support "variable modifiers" (v.var_modifiers = [ ]);
        tr_variable v
      end in
  var, tr_initializer_opt env v.var_initializer

let rec tr_statement (env: env) (s: statement) =
  match s with
  | Local_var vd ->
      let var = tr_var_decl false env vd in
      Sc_var_decl var, add_local_var (fst var) env
  | If_then (e, s) ->
      Sc_if (tr_expr_e env e, tr_block env [ s ], [ ]), env
  | If_then_else (e, s0, s1) ->
      Sc_if (tr_expr_e env e, tr_block env [ s0 ], tr_block env [ s1 ]), env
  | While (e, s0) ->
      Sc_while (tr_expr_e env e, tr_block env [ s0 ]), env
  | Expression (Assignment assign, _) ->
      begin
	match assign with
	| (Variable v, _), None, e ->
	    Sc_assign (tr_variable_name env v, tr_expr_e env e), env
	| _ -> non_supported "complex assignment"
      end
  | Expression _ -> non_supported "non-assignment expression"
  | Block_statement b ->
      Sc_block (tr_block env b), env
  | Return None ->
      Sc_return None, env
  | Return (Some e) ->
      Sc_return (Some (tr_expr_e env e)), env
  | Assert (e, None) -> 
      Sc_assert (tr_expr_e env e), env
  | Assert (_, _) -> non_supported "assert(e,e)"
  | Type_decl _ -> non_supported "type declaration"
  | Labeled _ -> non_supported "labeled statement"
  | For _ -> non_supported "for statement"
  | Foreach _ -> non_supported "for each"
  | Do_while _ -> non_supported "do while"
  | Empty_statement -> non_supported "empty statement"
  | Switch _ -> non_supported "switch statement"
  | Break _ -> non_supported "break statement"
  | Continue _ -> non_supported "continue statement"
  | Throw _ -> non_supported "throw execption"
  | Synchronized_statement _ -> non_supported "synchronized"
  | Try _ -> non_supported "try statement"

and tr_statement_e (env: env) (s, ext): s_command_e * env =
  let c, n_env = tr_statement env s in
  (c, ext), n_env

and tr_block (env: env) (b: block) =
  let inner_env = { env with e_locs = StringMap.empty :: env.e_locs } in
  let sb, _ =
    List.fold_left
      (fun (b_acc, e_acc) st ->
        let stat, ne = tr_statement_e e_acc st in
        stat :: b_acc, ne
      ) ([ ], inner_env) b in
  List.rev sb

let tr_method_declaration (env: env) (m: method_declaration) =
  check_support "non static methods" (List.mem Static m.method_modifiers);
  check_support "method modifiers" (List.length m.method_modifiers = 1);
  check_support "method exceptions" (m.method_throws = [ ]);
  let fp, env =
    List.fold_left
      (fun (a_pars, a_env) p ->
        let par = tr_formal_parameter p in
        par :: a_pars, add_local_var par a_env
      ) ([ ], { env with e_locs = StringMap.empty :: env.e_locs })
      m.method_parameters in
  { s_fun_name   = fst m.method_name ;
    s_fun_type   = tr_type m.method_type ;
    s_fun_params = List.rev fp ;
    s_fun_body   = tr_block env m.method_body ; }

let tr_class_body_decl (env: env) = function
  | Class_member member ->
      begin
	match member with
	| Field_decl f ->
            Sd_var (tr_var_decl true env f), env
	| Method_decl m ->
            let loc_env =
              { env with
                e_locs = StringMap.empty :: env.e_locs } in
	    Sd_function (tr_method_declaration loc_env m), env
	| Interface_decl _ -> non_supported "interface declaration"
	| Enum_decl _ -> non_supported "enum declaration in class"
	| Class_decl _ -> non_supported "class declaration in class"
      end
  | Static_init _ -> non_supported "static initializers"
  | Constructor_decl _ -> non_supported "constructors"
  | Block _ -> non_supported "blocks in class"
let tr_class_decl (env: env) (cd: class_decl) =
  check_support "class modifiers" (cd.class_modifiers = [ ]);
  check_support "class inherits" (cd.class_inherits = None);
  check_support "class interfaces" (cd.class_interfaces = [ ]);
  let decls, n_se =
    List.fold_left
      (fun (acc_decls, acc_se) d ->
	let sd, n_se = tr_class_body_decl acc_se d in
	sd :: acc_decls, n_se
      ) ([ ], { env with e_class = fst cd.class_name }) cd.class_body in
  { s_class_name = fst cd.class_name ;
    s_class_body = List.rev decls },
  { n_se with e_class = "" }
let tr_type_decl se = function
  | Class_declaration cd -> tr_class_decl se cd
  | Enum_declaration _ -> non_supported "enum types"
  | Interface_declaration _ -> non_supported "interfaces"
let tr_java_prog (jp: java_prog): s_program =
  check_support "packages" (jp.package = None);
  check_support "import" (jp.import = []);
  let env = generate_env jp in
  fst
    (List.fold_left
       (fun (prog, se) d ->
	 let c0, se0 = tr_type_decl se d in c0 :: prog, se0)
       ([ ], env) jp.type_decls)

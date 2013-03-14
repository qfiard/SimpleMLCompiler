open ML_syntax;;
open Printf;;
open Types;;
open Expression;;
open Char;;

let op_to_string = function
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Div -> "/"
    | And -> "&&"
    | Or -> "||"
    | Eq -> "=="
    | Neq -> "!="
    | Ge -> ">="
    | Gt -> ">"
    | Le -> "<="
    | Lt -> "<";;

let print_var = function
    | (s,i) -> printf "(%s,%d)" s i

let print_var_def = function
    | (s,i) -> printf "%s" s

let print_const = function
    | Int c -> printf "%d" c
    | Bool true -> printf "true"
    | Bool false -> printf "false"
    | Var v -> print_var v;;

let rec outputProgramWithIndentLevel indent = function
    | Local(s,e,scope) ->
        output_local indent s e scope
    | RecFun(f,arg,body,scope) ->
        output_rec_fun indent f arg body scope
    | Fun(arg,body) ->
        output_fun indent indent arg body
    | Eval(e1,e2) ->
        output_eval indent e1 e2
    | Binary(op,e1,e2) ->
        output_binary indent op e1 e2
    | If(cond,e1,e2) ->
        output_cond indent cond e1 e2
    | Const c -> 
        printf "%s" indent;
        print_const c
and outputWithParen indent e =
    let incr_indent = indent^"\t" in
    match e with
    | Const c ->
        printf "(";
        print_const c;
        printf ")"
    | _ ->
        printf "(\n";
        outputProgramWithIndentLevel incr_indent e;
        printf "\n%s)" indent;
and output_local indent s e scope =
    let incr_indent = indent^"\t" in
    printf "%slet %s =" indent s;
    begin
    match e with
    | Const c ->
        printf " ";
        print_const c;
    | _ ->
        printf "\n%sbegin" indent;
        outputProgramWithIndentLevel incr_indent e;
        printf "\n%send" indent;
    end;
    printf " in\n";
    outputProgramWithIndentLevel incr_indent scope
and output_rec_fun indent f arg body scope =
    let incr_indent = indent^"\t" in
    printf "%slet rec %s %s =\n" indent f arg;
    outputProgramWithIndentLevel incr_indent body;
    printf "\n%sin\n" indent;
    outputProgramWithIndentLevel incr_indent scope
and output_fun indent indent arg body =
    let incr_indent = indent^"\t" in
    printf "%sfun %s ->\n" indent arg;
    outputProgramWithIndentLevel incr_indent body
and output_eval indent e1 e2 =
    (* let incr_indent = indent^"\t" in *)
    printf "%s" indent;
    begin
    match e1 with
    | Const c1 ->
        print_const c1;
        printf " ";
    | _ -> outputWithParen indent e1;
    end;
    printf " ";
    match e2 with
    | Const c2 ->
        print_const c2;
    | _ -> outputWithParen indent e2
and output_binary indent op e1 e2 =
    printf "%s" indent;
    begin
        match e1 with
        | Const c1 ->
            print_const c1;
        | _ -> outputWithParen indent e1
    end;
    printf " %s " (op_to_string op);
    begin
        match e2 with
        | Const c2 ->
            print_const c2;
        | _ -> outputWithParen indent e2
    end
and output_cond indent cond e1 e2 =
    let incr_indent = indent^"\t" in
    printf "%sif" indent;
    outputWithParen incr_indent cond;
    printf "\n%sthen\n%sbegin\n" indent indent;
    outputProgramWithIndentLevel incr_indent e1;
    printf "\n%send\n%selse\n%sbegin\n" indent indent indent;
    outputProgramWithIndentLevel incr_indent e2;
    printf "\n%send" indent;;

let outputAbstractMachineValue = function
    | AbstractMachine.Int i -> printf "Integer %d" i;
    | AbstractMachine.Bool true -> printf "Boolean true";
    | AbstractMachine.Bool false -> printf "Boolean false";
    | _ -> printf "Closure"

let rec outputInstruction indent = function
    | AbstractMachine.Access i -> printf "%sAccess %d" indent i
    | AbstractMachine.Apply -> printf "%sApply" indent
    | AbstractMachine.Cur l ->
        printf "%sCur\n" indent;
        outputInstructionList (indent^"\t") l;
    | AbstractMachine.Return -> printf "%sReturn" indent
    | AbstractMachine.Let -> printf "%sLet" indent
    | AbstractMachine.Endlet -> printf "%sEndlet" indent
    | AbstractMachine.Branchneg n -> printf "%sBranchneg %d" indent n
    | AbstractMachine.Branch n -> printf "%sBranch %d" indent n
    | AbstractMachine.Op op -> printf "%sOp %s" indent (op_to_string op)
    | AbstractMachine.Push v -> printf "%sPush " indent; outputAbstractMachineValue v
and outputInstructionList indent = function
    | [] -> ()
    | x::[] -> outputInstruction indent x
    | x::s ->  outputInstruction indent x; printf "\n"; outputInstructionList indent s

let outputProgram p =
    printf "Compiled program :\n";
    begin
    match p with
    | Raw e ->  outputProgramWithIndentLevel "" (Raw2DBE.updateDeBruijnIndexes e)
    | DBE e -> printf "DBE expression"
    | Code e -> outputInstructionList "" e
    end;
    printf "\n\n"

let print_dbe_const = function
    | DeBruijnExpression.Int c -> printf "Integer %d" c
    | DeBruijnExpression.Bool true -> printf "Boolean true"
    | DeBruijnExpression.Bool false -> printf "Boolean false"
    | DeBruijnExpression.Var v -> printf "Variable %d" v

let outputValue = function
    | Interpreter.ConstVal c -> print_dbe_const c
    | Interpreter.FunVal f -> printf "Function : "; outputProgram f

let outputType t =
    let rec outputTypeAux = function
    | IntType -> printf "int"
    | BoolType -> printf "bool"
    | GenericType i ->
        printf "'";
        print_char(chr ((code 'a')+i))
    | FunType(FunType(t1,t2),FunType(t3,t4)) ->
        printf "(";
        outputTypeAux(FunType(t1,t2));
        printf ") -> (";
        outputTypeAux(FunType(t3,t4));
        printf ")";
    | FunType(FunType(t1,t2),t3) ->
        printf "(";
        outputTypeAux(FunType(t1,t2));
        printf ") -> ";
        outputTypeAux(t3);
    | FunType(t1,FunType(t3,t4)) ->
        outputTypeAux(t1);
        printf " -> (";
        outputTypeAux(FunType(t3,t4));
        printf ")";
    | FunType(t1,t2) ->
        outputTypeAux(t1);
        printf " -> ";
        outputTypeAux(t2);
    
    in
    printf "Type : ";
    outputTypeAux t;
    printf "\n"
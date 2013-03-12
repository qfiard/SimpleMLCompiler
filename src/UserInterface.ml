open ML_syntax;;
open Printf;;
open Types;;
open Expression;;

let op_to_string = function
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Div -> "/"
    | And -> "&&"
    | Or -> "||";;

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

let outputProgram p =
    printf "Compiled program :\n";
    match p with
    | Raw e -> outputProgramWithIndentLevel "" e
    | _ -> raise(Failure "Not implemented")
    printf "\n\n";;

let print_dbe_const = function
    | DeBruijnExpression.Int c -> printf "Integer : %d" c
    | DeBruijnExpression.Bool true -> printf "Boolean : true"
    | DeBruijnExpression.Bool false -> printf "Boolean : false"
    | DeBruijnExpression.Var v -> printf "Variable : %d" v;;

let outputValue = function
    | Interpreter.ConstVal c -> print_dbe_const c
    | Interpreter.FunVal f -> printf "Function : "; outputProgram f;;
open Syntax.ML_syntax;;
open Printf;;

let op_to_string = function
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Div -> "/"
    | And -> "&&"
    | Or -> "||";;

let print_const = function
    | Int c -> printf "%d" c
    | Bool true -> printf "true"
    | Bool false -> printf "false"
    | Id s -> printf "%s" s;;

let rec outputProgramWithIndentLevel indent = function
    | Local(s,e,scope) ->
        output_local indent s e scope
    | RecFun(fun_name,arg_name,body,scope) ->
        output_rec_fun indent fun_name arg_name body scope
    | Fun(arg_name,body) ->
        output_fun indent indent arg_name body
    | Eval(e1,e2) ->
        output_eval indent e1 e2
    | Binary(op,e1,e2) ->
        output_binary indent op e1 e2
    | Const c -> print_const c
and outputWithParen indent e =
    let incr_indent = indent^"\t" in
    printf "(\n";
    outputProgramWithIndentLevel incr_indent e;
    printf "\n%s)" indent;
and output_local indent s e scope =
    let incr_indent = indent^"\t" in
    printf "%slet %s =\n" indent s;
    outputProgramWithIndentLevel incr_indent e;
    printf "\n%sin\n" indent;
    outputProgramWithIndentLevel incr_indent scope
and output_rec_fun indent fun_name arg_name body scope =
    let incr_indent = indent^"\t" in
    printf "%slet rec %s %s =\n" indent fun_name arg_name;
    outputProgramWithIndentLevel incr_indent body;
    printf "\n%sin\n" indent;
    outputProgramWithIndentLevel incr_indent scope
and output_fun indent indent arg_name body =
    let incr_indent = indent^"\t" in
    printf "%sfun %s ->\n" indent arg_name;
    outputProgramWithIndentLevel incr_indent body
and output_eval indent e1 e2 =
    let incr_indent = indent^"\t" in
    printf "%s" indent;
    outputWithParen indent e1;
    match e2 with
    | Binary _ -> outputProgramWithIndentLevel incr_indent e2;
    | Const c -> print_const c;
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
        match e1 with
        | Const c1 ->
            print_const c1;
        | _ -> outputWithParen indent e2
    end;;

let outputProgram p =
    printf "Program Start\n";
    outputProgramWithIndentLevel "" p;
    printf "\nProgram End\n\n";;
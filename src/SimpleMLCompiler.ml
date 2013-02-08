open UserInterface;;
open ML_syntax;;

open Printf;;
open Pervasives;;

let file_path = Sys.argv.(1) in
let file = open_in file_path in

let print_file f : unit =
    let rec aux_print_file f first : unit =
        try
            if(first)
            then
                print_newline();
            print_string (input_line f);
            aux_print_file f false
        with eof ->
            () in
            
    seek_in f 0;
    aux_print_file f true in
    
(* Pass 1 *)
(* We transform boolean operators into conditionnal expressions *)
    
let rec binaryBoolOpToCond = function
    | Local(id,e,scope) -> Local(id,binaryBoolOpToCond e,binaryBoolOpToCond scope)
    | RecFun(id,arg,body,scope) ->
        RecFun(id,arg,binaryBoolOpToCond body,binaryBoolOpToCond scope)
    | Fun(arg,body) -> Fun(arg,binaryBoolOpToCond body)
    | Eval(e1,e2) -> Eval(binaryBoolOpToCond e1,binaryBoolOpToCond e2)
    | Binary(op,e1,e2) when op==And ->
        If(binaryBoolOpToCond e1,binaryBoolOpToCond e2,Const (Bool false))
    | Binary(op,e1,e2) when op==Or ->
        If(binaryBoolOpToCond e1,Const (Bool true),binaryBoolOpToCond e2)
    | Binary(op,e1,e2) -> Binary(op,binaryBoolOpToCond e1,binaryBoolOpToCond e2)
    | _ as ast -> ast
    in

let pass1 = binaryBoolOpToCond in
    
let compile file =
    let lexbuf = Lexing.from_channel file in
    let ast = ref (Parser.program Lexer.token lexbuf) in
    ast := pass1 !ast;
    !ast in
    
let p = compile file in
    printf "Input program :";
    print_file file;
    printf "\n\n";
    outputProgram p;;
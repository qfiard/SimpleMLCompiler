open UserInterface;;

open Printf;;
open Pervasives;;

open Interpreter;;
open Types;;

open Expression;;

let file_path = Sys.argv.(1) in
let file = open_in file_path in

let print_file f : unit =
    let rec aux_print_file f first : unit =
        try
            if(not first)
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
    | ML_syntax.Local(v,e,scope) -> ML_syntax.Local(v,binaryBoolOpToCond e,binaryBoolOpToCond scope)
    | ML_syntax.RecFun(f,arg,body,scope) ->
        ML_syntax.RecFun(f,arg,binaryBoolOpToCond body,binaryBoolOpToCond scope)
    | ML_syntax.Fun(arg,body) -> ML_syntax.Fun(arg,binaryBoolOpToCond body)
    | ML_syntax.Eval(e1,e2) -> ML_syntax.Eval(binaryBoolOpToCond e1,binaryBoolOpToCond e2)
    | ML_syntax.Binary(op,e1,e2) when op==And ->
        ML_syntax.If(binaryBoolOpToCond e1,binaryBoolOpToCond e2,ML_syntax.Const (ML_syntax.Bool false))
    | ML_syntax.Binary(op,e1,e2) when op==Or ->
        ML_syntax.If(binaryBoolOpToCond e1,ML_syntax.Const (ML_syntax.Bool true),binaryBoolOpToCond e2)
    | ML_syntax.Binary(op,e1,e2) -> ML_syntax.Binary(op,binaryBoolOpToCond e1,binaryBoolOpToCond e2)
    | ML_syntax.If(cond,e1,e2) -> ML_syntax.If(binaryBoolOpToCond cond,binaryBoolOpToCond e1,binaryBoolOpToCond e2)
    | _ as ast -> ast
    in

let pass1 = function
    | Raw e -> Raw (binaryBoolOpToCond e)
    | e -> e in
    
(* Pass 2 *)
(* Two steps :*)
(*    - We compute De Bruijn indexes for leaf variables *)
(*    - We convert the resulting expression to the type de_bruijn_expression (ie we drop the names of the variables) *)

let pass2 = function
    | Raw e -> DBE (Raw2DBE.raw2dbe e)
    | e -> e
    in
    
let compile file =
    let lexbuf = Lexing.from_channel file in
    let ast = ref (Parser.program Lexer.token lexbuf) in
    ast := pass1 !ast;
    outputProgram !ast;
    ast := pass2 !ast;
    !ast in


printf "Input program :\n";
print_file file;
printf "\n\n";
compile file;;
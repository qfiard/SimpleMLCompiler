open UserInterface;;
open ML_syntax;;

open Printf;;
open Pervasives;;

let file_path = Sys.argv.(1) in
let file = open_in file_path in
let lexbuf = Lexing.from_channel file in

let rec aux_print_file f first : unit =
    try
        if(first)
        then
            print_newline();
        print_string (input_line f);
        aux_print_file f false
    with eof ->
        () in

let print_file f : unit =
    seek_in f 0;
    aux_print_file f true in
        
let p = Parser.program Lexer.token lexbuf in
    printf "Input program :";
    print_file file;
    printf "\n\n";
    outputProgram p;;
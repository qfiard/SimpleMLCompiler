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
    
let compile file =
    let lexbuf = Lexing.from_channel file in
    let ast = ref (Parser.program Lexer.token lexbuf) in
    !ast in
    
let p = compile file in
    printf "Input program :";
    print_file file;
    printf "\n\n";
    outputProgram p;;
        

    
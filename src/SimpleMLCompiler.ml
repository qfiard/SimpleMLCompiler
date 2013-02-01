open Utilities.UserInterface;;
open Syntax.ML_syntax;;

open Lexer.Lexer;;
open Parser.Parser;;

let parse () =
    try
        let lexbuf = Lexing.from_channel stdin in
        while true do
            Parser.input Lexer.token lexbuf
        done
     with eof -> exit 0
    
let _ = Printexc.print parse ();;
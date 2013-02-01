(* Lexical analyser for a simple ML subset *)
{
open Parser.Parser;;
}

let digit = ['0' - '9']
let non_digit = ['a'-'z' 'A'-'Z' '_']

let int = '-' ? digit+
let identifier = non_digit (digit | non_digit)*

rule token = parse
  | int as n { INT(int_of_string n) }
  | "true" { BOOL(true) }
  | "false" { BOOL(false) }
  | identifier as id { ID(id) }
  | "let" { LET }
  | "in" { IN }
  | "rec" { REC }
  | "fun" { FUN }
  | "->" { ARROW }
  | "=" { EQ }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "&&" { AND }
  | "||" { OR }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | eof  { EOF }
{
(* java_lexer.mll *)
(* Analyseur lexical sans verification des conditions de contexte pour le 
langage Java *)

open Java_parser
open Localizing

exception Eof 

}

(* constantes numÃ©riques *)
let octal_digit = ['0'-'7']
let octal_constant = '0' octal_digit*

let hex_prefix = "0x" | "0X"
let hex_digit = ['a'-'f' 'A'-'F' '0'-'9']
let hex_constant = hex_prefix hex_digit+
let hex_digit_sequence = hex_digit+

let digit = ['0' - '9']
let digit_sequence = digit+

let unsigned_suffix = ['u' 'U']
let long_suffix = ['l' 'L']
let long_long_suffix = "ll" | "LL"
let integer_suffix =
          (unsigned_suffix long_suffix?)
        | (unsigned_suffix long_long_suffix)
        | (long_suffix unsigned_suffix?)
        | (long_long_suffix unsigned_suffix?)

let floating_suffix = ['f' 'l' 'F' 'L']

let integer_constant =
        (digit_sequence | hex_constant | octal_constant)
        integer_suffix?

let fractional_constant =
          (digit_sequence? '.' digit_sequence)
        | (digit_sequence '.')

let sign = ['+' '-']

let exponent_part =
        ['e' 'E'] sign? digit_sequence
let decimal_floating_constant =
          (fractional_constant exponent_part? floating_suffix?)
        | (digit_sequence exponent_part floating_suffix?)

let hex_fractional_constant =
          (hex_digit_sequence? '.' hex_digit_sequence)
        | (hex_digit_sequence '.')
let binary_exponent_part =
        ['p' 'P'] sign? digit_sequence
let hex_floating_constant =
          (hex_prefix hex_fractional_constant
             binary_exponent_part? floating_suffix?)
        | (hex_prefix hex_digit_sequence
             binary_exponent_part? floating_suffix?)


let floating_constant = decimal_floating_constant | hex_floating_constant

(**** Character constants *)
let quote = '"'
let double_quote = '"'
let single_quote = '\''
let backslash = '\\'

let simple_escape_sequence =
  (backslash single_quote) | (backslash double_quote)
| (backslash '?') | (backslash backslash)
| (backslash 'a') | (backslash 'b')
| (backslash 'f') | (backslash 'n')
| (backslash 'r') | (backslash 't')
| (backslash 'v')

let octal_escape_sequence =
  backslash octal_digit
| backslash octal_digit octal_digit
| backslash octal_digit octal_digit octal_digit

let hex_escape_sequence =
  backslash "x" hex_digit+

let hex_quad = hex_digit hex_digit hex_digit hex_digit
let universal_character_name =
  backslash ['u' 'U'] hex_quad

let escape_sequence =
  simple_escape_sequence
| octal_escape_sequence
| hex_escape_sequence
| universal_character_name

let c_char = [^ '\'' '\\' '\n'] | escape_sequence

let character_constant =
  single_quote c_char* single_quote
| 'L' single_quote c_char* single_quote

(**** String constants *)
let s_char = [^ '"' '\\' '\n'] | escape_sequence

let string_literal =
  (quote s_char* quote)
| ('L' quote s_char* quote)

(*** Identifiers *)
let nondigit = ['_' 'a'-'z' 'A'-'Z']

let identifier_nondigit =
  nondigit | universal_character_name (* no other characters *)

let identifier =
  identifier_nondigit (identifier_nondigit | digit)*

let newline = "\n" | "\r" | "\r\n"

rule token = parse
  newline { next_line lexbuf; token lexbuf }
| [' ' '\t' ] {token lexbuf}
| "//" [^'\n' '\r']* ['\n' '\r'] {next_line lexbuf; token lexbuf}
| "/*" {comment lexbuf; token lexbuf}
| integer_constant {let s = Lexing.lexeme lexbuf and e = extent lexbuf in
                    INTEGER_LITERAL(s,e)}
| floating_constant {let s = Lexing.lexeme lexbuf and e = extent lexbuf in
                    FLOATING_POINT_LITERAL(s,e)}
| character_constant  {
        let s = Lexing.lexeme lexbuf and e = extent lexbuf in
          CHARACTER_LITERAL(String.sub s 1 ((String.length s)-2), e) }
| string_literal  {
        let s = Lexing.lexeme lexbuf and e = extent lexbuf in
          STRING_LITERAL(String.sub s 1 ((String.length s)-2), e) }
| "boolean" {BOOLEAN(extent lexbuf)}
| "byte" {BYTE(extent lexbuf)}
| "short" {SHORT(extent lexbuf)}
| "int" {INT(extent lexbuf)}
| "long" {LONG(extent lexbuf)}
| "char" {CHAR(extent lexbuf)}
| "float" {FLOAT(extent lexbuf)}
| "double" {DOUBLE(extent lexbuf)}
| "true" {BOOLEAN_LITERAL(true, extent lexbuf)}
| "false" {BOOLEAN_LITERAL(false, extent lexbuf)}
| "null" {NULL_LITERAL(extent lexbuf)}
| "++"             { PLUSPLUS ((), extent lexbuf) }
| "--"             { MINUSMINUS ((), extent lexbuf) }
| "<<=" {LSHIFTEQ(extent lexbuf)}
| ">>=" {RSHIFTEQ(extent lexbuf)}
| ">>>=" {URSHIFTEQ(extent lexbuf)}
| ">>>" {URSHIFT(extent lexbuf)}
| ">>" {RSHIFT(extent lexbuf)}
| "<<" {LSHIFT(extent lexbuf)}
| "<=" {LTEQ(extent lexbuf)}
| ">=" {GTEQ(extent lexbuf)}
| "==" {EQEQ(extent lexbuf)}
| "!=" {NOTEQ(extent lexbuf)}
| "&=" {ANDEQ(extent lexbuf)}
| "^=" {XOREQ(extent lexbuf)}
| "|=" {OREQ(extent lexbuf)}
| '<' {LT(extent lexbuf)}
| '>' {GT(extent lexbuf)}
| "&&"             { ANDAND ((), extent lexbuf) }
| "||"             { OROR ((), extent lexbuf) }
| "*=" {MULTEQ(extent lexbuf)}
| "/=" {DIVEQ(extent lexbuf)}
| "%=" {MODEQ(extent lexbuf)}
| "+=" {PLUSEQ(extent lexbuf)}
| "-=" {MINUSEQ(extent lexbuf)}
| "â¦"      { ELLIPSIS ((), extent lexbuf) }
| '?'              { QUESTION ((), extent lexbuf) }
| '&'              { AND ((), extent lexbuf) }
| '^'              { XOR ((), extent lexbuf) }
| '|'              { OR ((), extent lexbuf) }
| '+'              { PLUS ((), extent lexbuf) }
| '-'              { MINUS ((), extent lexbuf) }
| '~'              { COMP ((), extent lexbuf) }
| '!'              { NOT ((), extent lexbuf) }
| '/'              { DIV ((), extent lexbuf) }
| '%'              { MOD ((), extent lexbuf) }
| '['              { LBRACK ((), extent lexbuf) }
| ']'              { RBRACK ((), extent lexbuf) }
| '.'              { DOT ((), extent lexbuf) }
| ';'              { SEMICOLON ((), extent lexbuf) }
| '*'              { MULT ((), extent lexbuf) }
| ','              { COMMA ((), extent lexbuf) }
| '{'              { LBRACE ((), extent lexbuf) }
| '}'              { RBRACE ((), extent lexbuf) }
| '='              { EQ ((), extent lexbuf) }
| '('              { LPAREN ((), extent lexbuf) }
| ')'              { RPAREN ((), extent lexbuf) }
| ':'              { COLON ((), extent lexbuf) }
| "abstract"       { ABSTRACT ((), extent lexbuf) }
| "assert"         { ASSERT ((), extent lexbuf) }
| "break"          { BREAK ((), extent lexbuf) }
| "case"           { CASE ((), extent lexbuf) }
| "catch"          { CATCH ((), extent lexbuf) }
| "class"          { CLASS ((), extent lexbuf) }
| "continue"       { CONTINUE ((), extent lexbuf) }
| "default"        { DEFAULT ((), extent lexbuf) }
| "do"             { DO ((), extent lexbuf) }
| "enum"           { ENUM ((), extent lexbuf) }
| "finally"        { FINALLY ((), extent lexbuf) }
| "for"            { FOR ((), extent lexbuf) }
| "if"             { IF ((), extent lexbuf) }
| "implements"     { IMPLEMENTS ((), extent lexbuf) }
| "import"         { IMPORT ((), extent lexbuf) }
| "instanceof"     { INSTANCEOF ((), extent lexbuf) }
| "interface"      { INTERFACE ((), extent lexbuf) }
| "else"           { ELSE ((), extent lexbuf) }
| "extends"        { EXTENDS ((), extent lexbuf) }
| "final"          { FINAL ((), extent lexbuf) }
| "native"         { NATIVE ((), extent lexbuf) }
| "new"            { NEW ((), extent lexbuf) }
| "package"        { PACKAGE ((), extent lexbuf) }
| "private"        { PRIVATE ((), extent lexbuf) }
| "protected"      { PROTECTED ((), extent lexbuf) }
| "public"         { PUBLIC ((), extent lexbuf) }
| "return"         { RETURN ((), extent lexbuf) }
| "static"         { STATIC ((), extent lexbuf) }
| "strictfp"       { STRICTFP ((), extent lexbuf) }
| "super"          { SUPER ((), extent lexbuf) }
| "switch"         { SWITCH ((), extent lexbuf) }
| "synchronized"   { SYNCHRONIZED ((), extent lexbuf) }
| "this"           { THIS ((), extent lexbuf) }
| "throws"         { THROWS ((), extent lexbuf) }
| "throw"          { THROW ((), extent lexbuf) }
| "transient"      { TRANSIENT ((), extent lexbuf) }
| "try"            { TRY ((), extent lexbuf) }
| "void"           { VOID ((), extent lexbuf) }
| "volatile"       { VOLATILE ((), extent lexbuf) }
| "while"          { WHILE ((), extent lexbuf) }
| identifier       { IDENTIFIER (Lexing.lexeme lexbuf, extent lexbuf) }
| eof              { EOF }

and comment = parse
  "*/" { () }
| [^ '\n'] { comment lexbuf }
| newline { next_line lexbuf; comment lexbuf }


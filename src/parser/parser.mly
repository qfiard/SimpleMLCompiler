%{
(* java_parser.mly *)
(* syntaxe concrete de Java 1.5 *)

open Syntax.ML_syntax;;

let parse_error s = (* Called by the parser function on error *)
    print_endline s;
    flush stdout

%}

%token LPAREN RPAREN
%token AND OR PLUS MINUS TIMES DIV

%token <string> ID
%token <int> INT
%token <bool> BOOL

%token FUNCTION_CALL

%token LET REC EQ IN FUN ARROW
%token EOF

%nonassoc LET REC EQ IN FUN ARROW
%nonassoc ID INT BOOL
%nonassoc FUNCTION_CALL
%left AND OR
%left PLUS MINUS
%left TIMES DIV
%nonassoc LPAREN RPAREN

%start program
%type <ML_syntax.expression> program

%%
program:
    expression EOF { $1 }
;
expression_with_parentheses:
    LPAREN expression RPAREN { $2 }
;
leaf_node:
    BOOL  { Const (Bool $1) }
  | INT { Const (Int $1) }
  | ID { Const (Id $1) }
;
function_call:
    ID leaf_node { Eval(Const (Id $1),$2) }
  | expression_with_parentheses expression { Eval($1,$2) } %prec FUNCTION_CALL
;
expression:
  | FUN ID ARROW expression  { Fun(Id $2,$4) }
  | LET ID EQ expression IN expression { Local(Id $2,$4,$6) }
  | LET REC ID ID EQ expression IN expression { RecFun(Const (Id $3),Const (Id $4),$6,$8) }
  | function_call { $1 }
  | expression_with_parentheses { $1 }
  | expression PLUS expression { Binary(Plus,$1,$3) }
  | expression TIMES expression { Binary(Times,$1,$3) }
  | expression MINUS expression { Binary(Minus,$1,$3) }
  | expression DIV expression { Binary(Div,$1,$3) }
  | expression AND expression { Binary(And,$1,$3) }
  | expression OR expression { Binary(Or,$1,$3) }

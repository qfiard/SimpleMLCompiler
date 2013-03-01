%{
(* java_parser.mly *)
(* syntaxe concrete de Java 1.5 *)

open Types;;
open ML_syntax;;
open Expression;;
open Printf;;

let current_line = ref 0;;

let parse_error s = (* Called by the parser function on error *)
    printf "Parse error at line %i\n" (!current_line);
    flush stdout

%}

%token LPAREN RPAREN
%token AND OR PLUS MINUS TIMES DIV

%token <string> ID
%token <int> INT
%token <bool> BOOL

%token FUNCTION_CALL

%token LET REC EQ IN FUN ARROW
%token IF THEN ELSE
%token EOF

%nonassoc LET REC EQ IN FUN ARROW
%nonassoc ID INT BOOL
%nonassoc FUNCTION_CALL
%nonassoc IF THEN
%nonassoc ELSE
%left AND OR
%left PLUS MINUS
%left TIMES DIV
%nonassoc LPAREN RPAREN

%start program
%type <Expression.expression> program

%%
program:
    expression EOF { Raw $1 }
;
expression_with_parentheses:
    LPAREN expression RPAREN { $2 }
;
leaf_node:
    BOOL  { Const (Bool $1) }
  | INT { Const (Int $1) }
  | ID { Const (Var ($1,0)) }
;
function_call:
    ID leaf_node { Eval(Const (Var($1,0)),$2) }
  | expression expression { Eval($1,$2) } %prec FUNCTION_CALL
;
expression:
  | leaf_node { $1 }
  | FUN ID ARROW expression  { Fun($2,$4) }
  | LET REC ID ID EQ expression IN expression { RecFun($3,$4,$6,$8) }
  | LET ID EQ expression IN expression { Local($2,$4,$6) }
  | function_call { $1 }
  | expression_with_parentheses { $1 }
  | expression PLUS expression { Binary(Plus,$1,$3) }
  | expression TIMES expression { Binary(Times,$1,$3) }
  | expression MINUS expression { Binary(Minus,$1,$3) }
  | expression DIV expression { Binary(Div,$1,$3) }
  | expression AND expression { Binary(And,$1,$3) }
  | expression OR expression { Binary(Or,$1,$3) }

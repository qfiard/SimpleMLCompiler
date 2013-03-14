%{
(* java_parser.mly *)
(* syntaxe concrete de Java 1.5 *)

open Types;;
open ML_syntax;;
open Expression;;
open Printf;;

let parse_error s = (* Called by the parser function on error *)
    printf "Parse error";
    flush stdout

%}

%token LPAREN RPAREN
%token AND OR PLUS MINUS TIMES DIV EQ NEQ LE GE LT GT

%token <string> ID
%token <int> INT
%token <bool> BOOL

%token FUNCTION_CALL

%token LET REC ASSIGN IN FUN ARROW
%token IF THEN ELSE
%token EOF

%nonassoc LET REC ASSIGN IN FUN ARROW
%nonassoc ID INT BOOL
%nonassoc IF THEN ELSE
%right FUNCTION_CALL
%left AND OR
%nonassoc EQ NEQ LE GE LT GT
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
if_expression:
    IF expression THEN expression ELSE expression { If($2,$4,$6) }
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
  | if_expression { $1 }
  | leaf_node { $1 }
  | FUN ID ARROW expression  { Fun($2,$4) }
  | LET REC ID ID ASSIGN expression IN expression { RecFun($3,$4,$6,$8) }
  | LET ID ASSIGN expression IN expression { Local($2,$4,$6) }
  | function_call { $1 }
  | expression_with_parentheses { $1 }
  | expression PLUS expression { Binary(Plus,$1,$3) }
  | expression TIMES expression { Binary(Times,$1,$3) }
  | expression MINUS expression { Binary(Minus,$1,$3) }
  | expression DIV expression { Binary(Div,$1,$3) }
  | expression AND expression { Binary(And,$1,$3) }
  | expression OR expression { Binary(Or,$1,$3) }
  | expression EQ expression { Binary(Eq,$1,$3) }
  | expression NEQ expression { Binary(Neq,$1,$3) }
  | expression LE expression { Binary(Le,$1,$3) }
  | expression LT expression { Binary(Lt,$1,$3) }
  | expression GE expression { Binary(Ge,$1,$3) }
  | expression GT expression { Binary(Gt,$1,$3) }

%{
(* java_parser.mly *)
(* syntaxe concrete de Java 1.5 *)

open Localizing
open Java_syntax
(* Pour construire les types tableaux: *)
let lift_array t l = List.fold_left (fun t' () -> Array_type t') t l

%}

%token <unit * Localizing.extent>     ABSTRACT ASSERT BREAK
%token <unit * Localizing.extent>     CASE CATCH CLASS CONTINUE
%token <unit * Localizing.extent>     DEFAULT DO ELSE ENUM
%token <unit * Localizing.extent>     EXTENDS FINAL FINALLY
%token <unit * Localizing.extent>     FOR IF IMPLEMENTS IMPORT
%token <unit * Localizing.extent>     INSTANCEOF INTERFACE NATIVE
%token <unit * Localizing.extent>     NEW PACKAGE PRIVATE PROTECTED
%token <unit * Localizing.extent>     PUBLIC RETURN SYNCHRONIZED
%token <unit * Localizing.extent>     STATIC STRICTFP SUPER SWITCH
%token <unit * Localizing.extent>     THIS TRANSIENT TRY THROW THROWS
%token <unit * Localizing.extent>     VOID VOLATILE WHILE

%token <unit * Localizing.extent>     LBRACE RBRACE
%token <unit * Localizing.extent>     LBRACK RBRACK
%token <unit * Localizing.extent>     LPAREN RPAREN
%token <unit * Localizing.extent>     COLON COMMA SEMICOLON
%token <unit * Localizing.extent>     COMP NOT
%token <unit * Localizing.extent>     AND ANDAND DIV DOT EQ MINUS MOD MULT
%token <unit * Localizing.extent>     OR OROR PLUS QUESTION XOR
%token <unit * Localizing.extent>     PLUSPLUS MINUSMINUS
%token <unit * Localizing.extent>     ELLIPSIS

%token <Localizing.extent> BOOLEAN BYTE SHORT INT LONG CHAR FLOAT DOUBLE
%token <Localizing.extent> LSHIFT RSHIFT URSHIFT LT GT LTEQ GTEQ EQEQ NOTEQ
%token <Localizing.extent> MULTEQ DIVEQ MODEQ PLUSEQ MINUSEQ LSHIFTEQ RSHIFTEQ URSHIFTEQ
%token <Localizing.extent> ANDEQ XOREQ OREQ NULL_LITERAL
%token <string*Localizing.extent> INTEGER_LITERAL IDENTIFIER FLOATING_POINT_LITERAL
%token <bool*Localizing.extent>   BOOLEAN_LITERAL
%token <string*Localizing.extent> CHARACTER_LITERAL STRING_LITERAL
%token EOF

%start program
%type <Java_syntax.java_prog>        program
%type<Java_syntax.variable list>     field_declaration
%type<Java_syntax.switch_block list> switch_block_statement_groups
%type<Java_syntax.switch_block>      switch_block_statement_group
%type<Java_syntax.switch_label list> switch_labels
%type<Java_syntax.instance_creation> class_instance_creation_expression
%type<Java_syntax.assignment>        assignment
%type<Java_syntax.expr_e>            primary expression_nn
%type<Java_syntax.name*Localizing.extent> name



%type <class_body_decl * extent>        static_initializer
%type <class_decl * extent>             class_declaration
%type <class_body_decl list * extent>   class_body
%type <modifier list * extent option>   modifiers_opt
%type <modifier list * extent>          modifiers
%type <modifier * extent>               modifier

%type <(enum_constant list * class_body_decl list) * extent> enum_body
%type <enum_declaration * extent>       enum_declaration

%type <class_decl * extent>             interface_declaration
%type <class_body_decl list * extent>   interface_body

%type <statement_e list>          method_body
%type <class_body_decl list>      class_body_declarations
%type <class_body_decl list>      class_body_declaration

%type <statement_e list * extent>       block
%type <statement_e list>                block_statements_opt
%type <statement_e list>                block_statements
%type <statement_e list>                block_statement
%type <statement_e list>                local_variable_declaration_statement

%type <for_statement * extent>          for_statement
%type <foreach_statement * extent>      foreach_statement

%type <statement_e>                     statement
%type <statement_e>                     statement_no_short_if
%type <statement_e>                     statement_without_trailing_substatement
%type <statement_e>                     empty_statement
%type <switch_block list * extent>      switch_block

%type <(formal_parameter * block) list * extent> catches
%type <(formal_parameter * block) list> catches_opt
%type <statement_e list * extent>       finally

%%

program:
| package_declaration_opt 
  import_declarations_opt
  type_declarations_opt
  EOF
    { { package    = $1;
	import     = $2;
	type_decls = $3 } }

/* Structure lexicale */
literal:
| INTEGER_LITERAL { Integer_constant (Int64.of_string (fst $1)), 
                    snd $1 }
| FLOATING_POINT_LITERAL { Float_constant (float_of_string (fst $1)), snd $1 }
| BOOLEAN_LITERAL { Bool_constant (fst $1), snd $1 }
| CHARACTER_LITERAL { Char_constant (fst $1), snd $1 }
| STRING_LITERAL { String_constant (fst $1), snd $1 }
| NULL_LITERAL { Null, $1 }
    
    /* Types, Values, and Variables */
var_type:
| primitive_type { $1 }
| reference_type { $1 }

primitive_type:
| numeric_type { $1 }
| BOOLEAN { Boolean, $1 }
    ;
numeric_type:
| integral_type { Integer (fst $1), snd $1 }
| floating_point_type { Floating_point (fst $1), snd $1 }

integral_type:
| BYTE { Byte, $1 }
| SHORT { Short, $1 }
| INT { Int, $1 }
| LONG { Long, $1 }
| CHAR { Char, $1 }

floating_point_type:
| FLOAT {Float, $1} 
| DOUBLE {Double, $1}

reference_type:
| class_or_interface_type { Class_type(fst $1), snd $1 }
| array_type {$1}

type_variable:
| IDENTIFIER {}

class_or_interface:
| name { Simple_class_type(fst $1), snd $1 }
| class_or_interface LT type_argument_list_1 DOT name 
    { Unsupported_class_type, fromto $1 $5 }

class_or_interface_type:
| class_or_interface { $1 }
| class_or_interface LT type_argument_list_1
    { Unsupported_class_type, extent_unknown () }

class_type:
| class_or_interface_type {$1}

interface_type:
| class_or_interface_type {$1}

array_type:
| primitive_type dims 
    { lift_array (fst $1) (fst $2), fromto $1 $2 }
| name dims { lift_array (Class_type (Simple_class_type (fst $1))) 
                (fst $2), fromto $1 $2 }
| class_or_interface LT type_argument_list_1 DOT name dims
    { Array_type(Class_type Unsupported_class_type), fromto $1 $5 }
| class_or_interface LT type_argument_list_1 dims
    { Array_type(Class_type Unsupported_class_type), fromto $1 $4 }

/*type_arguments_opt: type_arguments | ;

type_arguments::=
		LT type_argument_list_1
*/

wildcard:
| QUESTION {}
| QUESTION EXTENDS reference_type {}
| QUESTION SUPER reference_type {}

wildcard_1:
| QUESTION GT {}
| QUESTION EXTENDS reference_type_1 {}
| QUESTION SUPER reference_type_1 {}

wildcard_2:
| QUESTION RSHIFT {}
| QUESTION EXTENDS reference_type_2 {}
| QUESTION SUPER reference_type_2 {}

wildcard_3:
| QUESTION URSHIFT {}
| QUESTION EXTENDS reference_type_3 {}
| QUESTION SUPER reference_type_3 {}

reference_type_1:
| reference_type GT {}
| class_or_interface LT type_argument_list_2 {}

reference_type_2:
| reference_type RSHIFT {}
| class_or_interface LT type_argument_list_3 {}
 
reference_type_3:
| reference_type URSHIFT {}

type_argument_list:
| type_argument {}
| type_argument_list COMMA type_argument {}

type_argument_list_1:
| type_argument_1 {}
| type_argument_list COMMA type_argument_1 {}

type_argument_list_2:
| type_argument_2 {}
| type_argument_list COMMA type_argument_2 {}

type_argument_list_3:
| type_argument_3 {}
| type_argument_list COMMA type_argument_3 {}

type_argument:
| reference_type {}
| wildcard {}

type_argument_1:
| reference_type_1 {}
| wildcard_1 {}

type_argument_2:
| reference_type_2 {}
| wildcard_2 {}

type_argument_3:
| reference_type_3 {}
| wildcard_3 {}


/* Names */
name:
| simple_name	{$1}
| qualified_name	{$1}

simple_name:
| IDENTIFIER {Simple_name $1, snd $1}

qualified_name:
| name DOT IDENTIFIER {Qualified_name(fst $1, (fst $3)), fromto $1 $3 }


/* Packages */
package_declaration_opt:
| package_declaration {Some $1} | {None};
import_declarations_opt:
| import_declarations {List.rev $1} | {[]};
type_declarations_opt:
| type_declarations  {List.rev $1} | {[]};

import_declarations: 
| import_declaration {[$1]}
| import_declarations import_declaration { $2::$1 }
	    
type_declarations: 
| type_declaration {[$1]}
| SEMICOLON {[]}
| type_declarations type_declaration { $2::$1 }
| type_declarations SEMICOLON { $1 }

package_declaration: 
| PACKAGE name SEMICOLON {fst $2}

import_declaration: 
| single_type_import_declaration 
    { { import_type   = Single_type_import;
	import_static = false;
	import_name   = $1 } }
| type_import_on_demand_declaration
    { { import_type   = Type_import_on_demand;
	import_static = false;
	 import_name  = $1 } }
| static_single_type_import_declaration
    { { import_type   = Single_type_import;
	import_static = true;
	import_name   = $1 } }
| static_type_import_on_demand_declaration
    { { import_type   = Type_import_on_demand;
	import_static = true;
	import_name   = $1 } }

single_type_import_declaration: 
| IMPORT name SEMICOLON {fst $2}

static_single_type_import_declaration: 
| IMPORT STATIC name SEMICOLON {fst $3 }

type_import_on_demand_declaration:
| IMPORT name DOT MULT SEMICOLON {fst $2}

static_type_import_on_demand_declaration:
| IMPORT STATIC name DOT MULT SEMICOLON {fst $3 }

type_declaration:
| class_declaration      { Class_declaration (fst $1) }
| enum_declaration       { Enum_declaration (fst $1) }
| interface_declaration  { Interface_declaration (fst $1) }


/* Productions used only in the LALR(1) grammar */
modifiers_opt:
|           { [], None }
| modifiers { List.rev (fst $1), Some (snd $1) }


modifiers:
| modifier           { [ fst $1 ], snd $1 }
| modifiers modifier { (fst $2) :: (fst $1), fromto $1 $2 }

modifier:
| PUBLIC       { Public,       snd $1 } 
| PROTECTED    { Protected,    snd $1 }
| PRIVATE      { Private,      snd $1 }
| STATIC       { Static,       snd $1 }
| ABSTRACT     { Abstract,     snd $1 }
| FINAL        { Final,        snd $1 }
| NATIVE       { Native,       snd $1 }
| SYNCHRONIZED { Synchronized, snd $1 }
| TRANSIENT    { Transient,    snd $1 }
| VOLATILE     { Volatile,     snd $1 }
| STRICTFP     { Strictfp,     snd $1 } /* note that semantic analysis must check 
    that the context of the modifier allows strictfp */


/* Classes */

/* Class Declaration */
class_declaration:
| CLASS IDENTIFIER type_parameters_opt
    super_opt interfaces_opt class_body
    { { class_modifiers  = [];
	class_name       = $2;
	class_inherits   = $4;
	class_interfaces = $5;
	class_body       = fst $6 },
      fromto $1 $6 }
| modifiers CLASS IDENTIFIER type_parameters_opt
    super_opt interfaces_opt class_body
    { { class_modifiers  = List.rev (fst $1);
	class_name       = $3;
	class_inherits   = $5;
	class_interfaces = $6;
	class_body       = fst $7 },
      fromto $1 $7 }

super:
| EXTENDS class_type { $2 }

super_opt:
| { None }
| super { Some (fst $1) }

interfaces:
| IMPLEMENTS interface_type_list {List.rev $2}

interfaces_opt:
| {[]}
| interfaces {$1}

interface_type_list: 
| interface_type {[fst $1]}
| interface_type_list COMMA interface_type { (fst $3)::$1 }

class_body:
| LBRACE class_body_declarations_opt RBRACE { $2, fromto $1 $3 }

class_body_opt:
|             { [] }
| class_body  { fst $1 }

class_body_declarations_opt:
| {[]}
| class_body_declarations {$1};

class_body_declarations: 
| class_body_declaration                         { $1 }
| SEMICOLON                                      { [] }
| class_body_declarations class_body_declaration { $1 @ $2 }
| class_body_declarations SEMICOLON              { $1 }
    
class_body_declaration:
| class_member_declaration  { List.map (fun cm -> Class_member cm) $1 }
| static_initializer        { [ fst $1 ] }
| constructor_declaration   { [ Constructor_decl $1 ] }
| block                     { [ Block (fst $1) ] }

class_member_declaration:
| field_declaration {List.map (fun v -> Field_decl v) $1}
| method_declaration {[Method_decl $1]}
    /* repeat the prod for 'class_declaration' here: */
| modifiers_opt CLASS IDENTIFIER type_parameters_opt super_opt
  interfaces_opt class_body
    { [ Class_decl { class_modifiers  = fst $1;
                     class_name       = $3;
                     class_inherits   = $5;
                     class_interfaces = $6;
                     class_body       = fst $7 } ] }
| enum_declaration
    { [ Enum_decl (fst $1) ] }
| interface_declaration
    { [ Interface_decl (fst $1) ] }
    

/* Enum Declaration */
enum_declaration:
| modifiers_opt ENUM IDENTIFIER interfaces_opt enum_body
    { { enum_modifiers  = fst $1;
	enum_name       = fst $3;
	enum_interfaces = $4;
	enum_body       = fst $5 },
      fromto ((), extopt (snd $1) (snd $2)) $5 }

enum_body:
| LBRACE enum_constants_opt enum_body_declarations_opt RBRACE
    { ($2, $3), fromto $1 $4 }

enum_constants_opt:
| {[]}
| enum_constants {List.rev $1}

enum_constants:
| enum_constant {[$1]}
| enum_constants COMMA enum_constant { $3::$1 }

enum_constant:
| IDENTIFIER enum_arguments_opt
    { { enum_constant_name = fst $1;
        enum_constant_args = $2;
	enum_constant_body = [] } }
| IDENTIFIER enum_arguments_opt class_body
    { { enum_constant_name = fst $1;
	enum_constant_args = $2;
	enum_constant_body = fst $3 } }

enum_arguments_opt:
| {[]}
| LPAREN argument_list_opt RPAREN { $2 }

enum_body_declarations_opt:
|{[]}
| SEMICOLON class_body_declarations_opt { $2 }


/* Field Declarations */
field_declaration: 
| modifiers_opt var_type variable_declarators SEMICOLON 
    { List.rev_map
	(fun (n,i) -> { var_modifiers   = fst $1;
                        var_type        = fst $2;
                        var_name        = n;
                        var_initializer = i } ) $3 }
    
variable_declarators:
| variable_declarator                            { [ $1 ] }
| variable_declarators COMMA variable_declarator { $3::$1 }

variable_declarator:
| variable_declarator_id                         { $1, None }
| variable_declarator_id EQ variable_initializer { $1, Some $3 }

variable_declarator_id:
| IDENTIFIER                            { $1 }
| variable_declarator_id LBRACK RBRACK  { $1 }

variable_initializer:
| expression            { Expr_init $1 }
| array_initializer     { Array_init (fst $1) }

/* Method Declarations */
method_declaration:
| method_header method_body { let m,t,n,th = $1 in
	{ method_modifiers = m;
	  method_type = t;
	  method_throws = th;
	  method_name = fst n;
	  method_parameters = snd n;
	  method_body = $2 } }
    
method_header:
| modifiers_opt var_type method_declarator throws_opt 
    { (fst $1, fst $2, $3, $4) }
/*| modifiers_opt LT type_parameter_list_1 var_type method_declarator throws_opt*/
| modifiers_opt VOID method_declarator throws_opt
    { (fst $1, Void, $3, $4) }
/*| modifiers_opt LT type_parameter_list_1 VOID method_declarator throws_opt*/
      
method_declarator:
| IDENTIFIER LPAREN formal_parameter_list_opt RPAREN {$1, $3 }

formal_parameter_list_opt:
| {[]}
| formal_parameter_list {List.rev $1}

formal_parameter_list:
| formal_parameter {[$1]}
| formal_parameter_list COMMA formal_parameter { $3::$1 }

formal_parameter:
| var_type variable_declarator_id 
    { { param_type = fst $1;
	param_name = $2;
	param_is_final = false;
	param_is_ellipsis = false } }
| FINAL var_type variable_declarator_id
    { { param_type = fst $2;
	param_name = $3;
	param_is_final = true;
	param_is_ellipsis = false } }
/* careful, productions below allow varargs in non-final positions. */
| var_type ELLIPSIS IDENTIFIER
    { { param_type = fst $1;
	param_name = $3;
	param_is_final = false;
	param_is_ellipsis = true } }
| FINAL var_type ELLIPSIS IDENTIFIER
    { { param_type = fst $2;
	param_name = $4;
	param_is_final = true;
	param_is_ellipsis = true } }

throws_opt:
| {[]}
| throws {$1}

throws:
| THROWS class_type_list { List.rev $2 }

class_type_list:
| class_type {[fst $1]}
| class_type_list COMMA class_type { (fst $3)::$1 }

method_body:
| block      { fst $1 }
| SEMICOLON  { [ ] }


/* Static Initializers */
static_initializer:
| STATIC block  { Static_init (fst $2), fromto $1 $2 }


/* Constructor Declarations */
constructor_declaration:
| modifiers_opt constructor_declarator
    throws_opt constructor_body
    { let n,fp = $2 and ci,b = $4 in 
      { constructor_modifiers  = fst $1;
	constructor_throws     = $3;
	constructor_name       = n;
	constructor_parameters = fp;
	constructor_invocation = ci;
	constructor_body = b } }
/*| modifiers_opt LT type_parameter_list_1 constructor_declarator
  throws_opt constructor_body*/

constructor_declarator:
| simple_name LPAREN formal_parameter_list_opt RPAREN {fst $1, $3 }

constructor_body:
| LBRACE explicit_constructor_invocation
    block_statements RBRACE
    { Some $2, $3 }
| LBRACE explicit_constructor_invocation RBRACE
    { Some $2, []}
| LBRACE block_statements RBRACE {None, $2}
| LBRACE RBRACE {None, []}

explicit_constructor_invocation:
| THIS LPAREN argument_list_opt RPAREN SEMICOLON
    { This_constructor, $3 }
/*| type_arguments THIS LPAREN argument_list_opt RPAREN SEMICOLON*/
| SUPER LPAREN argument_list_opt RPAREN SEMICOLON
    { Super_constructor, $3 }
/*| type_arguments SUPER LPAREN argument_list_opt RPAREN SEMICOLON*/
| primary DOT SUPER LPAREN argument_list_opt RPAREN SEMICOLON
    {Super_const_of_expr $1, $5}
/*| primary DOT type_arguments SUPER LPAREN argument_list_opt RPAREN SEMICOLON*/
| name DOT SUPER LPAREN argument_list_opt RPAREN SEMICOLON
    { Super_const_of_name (fst $1), $5 }
/*| name DOT type_arguments SUPER LPAREN argument_list_opt RPAREN SEMICOLON*/


/* Interfaces */

/* Interface Declarations */
interface_declaration:
| modifiers_opt INTERFACE IDENTIFIER type_parameters_opt
    extends_interfaces_opt interface_body
    { { class_modifiers  = fst $1;
        class_name       = $3;
        class_inherits   = None;
        class_interfaces = $5;
        class_body       = fst $6 },
      fromto ((), extopt (snd $1) (snd $2)) $6 }

extends_interfaces_opt:
| {[]}
| extends_interfaces {List.rev $1}
    
extends_interfaces:
| EXTENDS interface_type {[fst $2]}
| extends_interfaces COMMA interface_type { (fst $3)::$1 }

interface_body:
| LBRACE interface_member_declarations_opt RBRACE { $2, fromto $1 $3 }

interface_member_declarations_opt:
| {[]}
| interface_member_declarations {$1}

interface_member_declarations:
| interface_member_declaration {$1}
| SEMICOLON {[]}
| interface_member_declarations interface_member_declaration
    { $1@$2 }
| interface_member_declarations SEMICOLON {$1}
    
interface_member_declaration:
| constant_declaration { List.map (fun v -> Class_member(Field_decl v) ) $1 }
| abstract_method_declaration { [ Class_member $1 ] }
| class_declaration           { [ Class_member (Class_decl (fst $1)) ] }
| enum_declaration            { [ Class_member (Enum_decl (fst $1)) ] }
| interface_declaration       { [ Class_member (Interface_decl (fst $1)) ] }

constant_declaration:
| field_declaration {$1}
    /* need to semantically check that modifiers of field declaration
      include only PUBLIC, STATIC, or FINAL.  Other modifiers are
	disallowed. */
      
abstract_method_declaration:
| method_header SEMICOLON
    { let m,t,n,th = $1 in
      Method_decl { method_modifiers = m;
                    method_type = t;
                    method_throws = th;
                    method_name = fst n;
                    method_parameters = snd n;
                    method_body = [] } }


/* Arrays */
array_initializer:
| LBRACE variable_initializers COMMA RBRACE { $2, fromto $1 $4 }
| LBRACE variable_initializers RBRACE       { $2, fromto $1 $3 }
/*	| LBRACE COMMA RBRACE
| LBRACE RBRACE*/

variable_initializers:
| variable_initializer {[$1]}
| variable_initializers COMMA variable_initializer {$1@[$3]}


/* Blocks and Statements */
block:
| LBRACE block_statements_opt RBRACE { $2, fromto $1 $3 }

block_statements_opt:
| {[]}
| block_statements { $1}

block_statements:
| block_statement                  { $1 }
| block_statements block_statement { $1 @ $2 }

block_statement:
| local_variable_declaration_statement { $1 }
| statement             { [ $1 ] }
| class_declaration     { [ Type_decl (Class_declaration (fst $1)), snd $1 ] }
| enum_declaration      { [ Type_decl (Enum_declaration (fst $1)), snd $1 ] }
| interface_declaration { [ Type_decl (Interface_declaration (fst $1)), snd $1 ] }

local_variable_declaration_statement:
| local_variable_declaration SEMICOLON 
    { List.map (fun (lvd,e) -> Local_var lvd, e) $1 }

local_variable_declaration:
| var_type variable_declarators
    { List.rev_map
        (fun (n,i) ->
          { var_modifiers   = [];
            var_type        = fst $1;
            var_name        = n;
            var_initializer = i },
          snd n ) $2 }
| FINAL var_type variable_declarators
    { List.rev_map
        (fun (n,i) ->
          { var_modifiers   = [Final];
            var_type        = fst $2;
            var_name        = n;
            var_initializer = i },
          snd n ) $3 }

statement:
| statement_without_trailing_substatement
                         { $1 }
| labeled_statement      { $1 }
| if_then_statement      { $1 }
| if_then_else_statement { $1 }
| while_statement        { $1 }
| for_statement          { For (fst $1), snd $1 }
| foreach_statement      { Foreach (fst $1), snd $1 }
    
statement_no_short_if:
| statement_without_trailing_substatement { $1 }
| labeled_statement_no_short_if           { $1 }
| if_then_else_statement_no_short_if      { $1 }
| while_statement_no_short_if             { $1 }
| for_statement_no_short_if               { For (fst $1), snd $1 }
| foreach_statement_no_short_if           { Foreach (fst $1), snd $1 }

statement_without_trailing_substatement:
| block                                   { Block_statement (fst $1), snd $1 }
| empty_statement                         { $1 }
| expression_statement                    { $1 }
| switch_statement                        { $1 }
| do_statement                            { $1 }
| break_statement                         { $1 }
| continue_statement                      { $1 }
| return_statement                        { $1 }
| synchronized_statement                  { $1 }
| throw_statement                         { $1 }
| try_statement                           { Try (fst $1), snd $1 }
| assert_statement                        { $1 }
    
empty_statement:
| SEMICOLON { Empty_statement, snd $1 }

labeled_statement:
| IDENTIFIER COLON statement              { Labeled ($1, $3), fromto $1 $3 }

labeled_statement_no_short_if:
| IDENTIFIER COLON statement_no_short_if  { Labeled ($1, $3), fromto $1 $3 }

expression_statement:
| statement_expression SEMICOLON          { Expression $1, fromto $1 $2 }

statement_expression:
| assignment                 { Assignment $1, let (e1,_,e2)=$1 in fromto e1 e2 }
| preincrement_expression    { $1 }
| predecrement_expression    { $1 }
| postincrement_expression   { $1 }
| postdecrement_expression   { $1 }
| method_invocation          { $1 }
| class_instance_creation_expression { New $1, extent_unknown () }

if_then_statement:
| IF LPAREN expression RPAREN statement { If_then ($3, $5), fromto $1 $5 }

if_then_else_statement:
| IF LPAREN expression RPAREN statement_no_short_if 
	ELSE statement
    { If_then_else ($3, $5, $7), fromto $1 $7 }

if_then_else_statement_no_short_if:
| IF LPAREN expression RPAREN statement_no_short_if
    ELSE statement_no_short_if
    { If_then_else ($3, $5, $7), fromto $1 $7 }

switch_statement:
| SWITCH LPAREN expression RPAREN switch_block
    { Switch ($3, fst $5), fromto $1 $5 }

switch_block:
| LBRACE switch_block_statement_groups switch_labels RBRACE
    { List.rev (($3,[]) :: $2), fromto $1 $4 }
| LBRACE switch_block_statement_groups RBRACE
    { List.rev $2, fromto $1 $3 }
| LBRACE switch_labels RBRACE
    { [$2,[]], fromto $1 $3 }
| LBRACE RBRACE
    { [], fromto $1 $2 }
    
switch_block_statement_groups:
| switch_block_statement_group {[$1]}
| switch_block_statement_groups switch_block_statement_group
    { $2::$1 }

switch_block_statement_group:
| switch_labels block_statements { $1, $2 }

switch_labels:
| switch_label {[$1]}
| switch_labels switch_label { $1@[$2] }

switch_label:
| CASE constant_expression COLON { Case $2 }
| DEFAULT COLON { Default }

while_statement:
| WHILE LPAREN expression RPAREN statement
    { While ($3, $5), fromto $1 $5 }

while_statement_no_short_if:
| WHILE LPAREN expression RPAREN statement_no_short_if
    { While ($3, $5), fromto $1 $5 }

do_statement:
| DO statement WHILE LPAREN expression RPAREN SEMICOLON
    { Do_while ($2, $5), fromto $1 $7 }

foreach_statement:
| FOR LPAREN var_type variable_declarator_id COLON expression RPAREN
    statement
    { { foreach_var = { var_modifiers   = [];
			var_type        = fst $3; 
                        var_name        = $4;
			var_initializer = None };
	foreach_val = $6;
	foreach_body = $8 },
      fromto $1 $8 }
| FOR IDENTIFIER LPAREN var_type variable_declarator_id IDENTIFIER
    expression RPAREN statement
    { if fst $2 = "each" && fst $6 = "in" then
      { foreach_var = { var_modifiers = [];
			var_type = fst $4; 
                        var_name = $5;
			var_initializer = None };
	foreach_val = $7;
	foreach_body = $9 },
      fromto $1 $9
    else failwith "Parse failure" }

foreach_statement_no_short_if:
| FOR LPAREN var_type variable_declarator_id COLON expression RPAREN
    statement_no_short_if
    { { foreach_var = { var_modifiers = [];
			var_type = fst $3; 
                        var_name = $4;
			var_initializer = None };
	foreach_val = $6;
	foreach_body = $8 },
      fromto $1 $8 }
| FOR IDENTIFIER LPAREN var_type variable_declarator_id IDENTIFIER
	expression RPAREN statement_no_short_if
    { if fst $2 = "each" && fst $6 = "in" then
      { foreach_var = { var_modifiers = [];
			var_type = fst $4; 
                        var_name = $5;
			var_initializer = None };
	foreach_val = $7;
	foreach_body = $9 },
      fromto $1 $9
      else failwith "Parse failure" }

for_statement:
| FOR LPAREN for_init_opt SEMICOLON expression_opt SEMICOLON
    for_update_opt RPAREN statement
    { { for_init = $3;
	for_test = $5;
	for_update = $7;
	for_body = $9 },
      fromto $1 $9}

for_statement_no_short_if:
| FOR LPAREN for_init_opt SEMICOLON expression_opt SEMICOLON
    for_update_opt RPAREN statement_no_short_if
    { { for_init = $3;
	for_test = $5;
	for_update = $7;
	for_body = $9 },
      fromto $1 $9 }

for_init_opt:
|          { For_expressions [] }
| for_init { $1 }

for_init:
| statement_expression_list  { For_expressions (List.rev $1) }
| local_variable_declaration { For_loc_var (List.map fst $1) }

for_update_opt:
| {[]}
| for_update {$1}

for_update:
| statement_expression_list {List.rev $1}

statement_expression_list:
| statement_expression                                  { [ $1 ] }
| statement_expression_list COMMA statement_expression  { $3 :: $1 }

identifier_opt:
|              { None }
| IDENTIFIER   { Some $1 }
        
break_statement:
| BREAK identifier_opt SEMICOLON { Break $2, fromto $1 $3 }

continue_statement:
| CONTINUE identifier_opt SEMICOLON { Continue $2, fromto $1 $3 }

return_statement:
| RETURN expression_opt SEMICOLON { Return $2, fromto $1 $3 }

throw_statement:
| THROW expression SEMICOLON { Throw $2, fromto $1 $3 }

synchronized_statement:
| SYNCHRONIZED LPAREN expression RPAREN block 
    { Synchronized_statement ($3, fst $5), fromto $1 $5 }

try_statement:
| TRY block catches 
    { { try_block  = fst $2;
        try_catch  = fst $3;
        try_finaly = None },
      fromto $1 $3 }
| TRY block catches_opt finally
    { { try_block  = fst $2;
        try_catch  = $3;
        try_finaly = Some (fst $4) },
      fromto $1 $4 }
    
catches_opt:
|         { [] }
| catches { fst $1 }

catches:
| catch_clause         { [ fst $1 ], snd $1 }
| catches catch_clause { (fst $1) @ [ fst $2 ], fromto $1 $2 }

catch_clause:
| CATCH LPAREN formal_parameter RPAREN block
     { ($3, (fst $5)), fromto $1 $5 }

finally:
| FINALLY block { $2 }

assert_statement:
| ASSERT expression SEMICOLON
    { Assert ($2, None), fromto $1 $3 }
| ASSERT expression COLON expression SEMICOLON 
    { Assert ($2, Some $4), fromto $1 $5 }

/* Expressions */
primary:
| primary_no_new_array  { $1 }
| array_creation_init   { $1 }
| array_creation_uninit { $1 }
    
primary_no_new_array:
| literal {$1}
| THIS                               { This, snd $1 }
| LPAREN name RPAREN                 { Variable (fst $2), fromto $1 $3 } 
| LPAREN expression_nn RPAREN        { fst $2, fromto $1 $3 }
| class_instance_creation_expression { New $1, extent_unknown () }
/* TODO extent */
| field_access {$1} 
| method_invocation {$1}
| array_access {$1}
/*	| name DOT THIS
| VOID DOT CLASS
| primitive_type DOT CLASS
| primitive_type dims DOT CLASS
| name DOT CLASS
| name dims DOT CLASS*/

class_instance_creation_expression:
| NEW class_or_interface_type LPAREN argument_list_opt RPAREN class_body_opt
    { { instance_type = fst $2;
	instance_type_args = [];
	instance_args = $4;
	instance_body = $6 } }
/*| NEW type_arguments class_or_interface_type LPAREN argument_list_opt RPAREN class_body_opt
{{instance_type = $3;
 instance_type_args = $2
 instance_args = $5;
 instance_body = $7 }}
| primary DOT NEW type_arguments_opt IDENTIFIER type_arguments_opt
    LPAREN argument_list_opt RPAREN class_body_opt
| name DOT NEW type_arguments_opt IDENTIFIER type_arguments_opt
    LPAREN argument_list_opt RPAREN class_body_opt*/

argument_list_opt:
| {[]}
| argument_list {List.rev $1}

argument_list:
| expression {[$1]}
| argument_list COMMA expression { $3::$1 }

array_creation_uninit:
| NEW primitive_type dim_exprs dims_opt
    { New_array { array_type = lift_array (fst $2) (fst $4);
                  array_dimensions = fst $3;
                  array_init = [] },
      fromtoopt $1 $3 $4 }
| NEW class_or_interface_type dim_exprs dims_opt
    { New_array { array_type = lift_array (Class_type(fst $2)) (fst $4);
                  array_dimensions = fst $3;
                  array_init = [] },
      fromtoopt $1 $3 $4 }

array_creation_init:
| NEW primitive_type dims array_initializer
    { New_array { array_type = lift_array (fst $2) (fst $3);
                  array_dimensions = [];
                  array_init = fst $4},
      fromto $1 $4 }
| NEW class_or_interface_type dims array_initializer
    { New_array { array_type = lift_array (Class_type(fst $2)) (fst $3);
                  array_dimensions = [];
                  array_init = fst $4 },
      fromto $1 $4 }
    
dim_exprs:
| dim_expr { [$1], snd $1 }
| dim_exprs dim_expr { (fst $1)@[$2], fromto $1 $2 }

dim_expr:
| LBRACK expression RBRACK  { fst $2, fromto $1 $3 }

dims_opt:
| {[], None}
| dims {fst $1, Some(snd $1)}

dims:
| LBRACK RBRACK        { [()], fromto $1 $2 }
| dims LBRACK RBRACK   { () :: (fst $1), fromto $1 $3 }

field_access:
| primary DOT IDENTIFIER
     { Field_access (Var_expr $1, (fst $3)), fromto $1 $3 }
| SUPER DOT IDENTIFIER
     { Field_access(Super, (fst $3)), fromto $1 $3 }
| name DOT SUPER DOT IDENTIFIER 
     { Field_access (Var_expr (Variable (fst $1), snd $1), (fst $5)), fromto $1 $5 }

method_invocation:
| name LPAREN argument_list_opt RPAREN 
    { Method_call (Named_method (fst $1), $3), fromto $1 $4 }
| primary DOT IDENTIFIER LPAREN argument_list_opt RPAREN
    { Method_call (Expr_method ($1,$3), $5), fromto $1 $6 }
/*	| primary DOT type_arguments IDENTIFIER LPAREN argument_list_opt RPAREN
| name DOT type_arguments IDENTIFIER LPAREN argument_list_opt RPAREN*/
| SUPER DOT IDENTIFIER LPAREN argument_list_opt RPAREN
    { Method_call (Super_method $3, $5), fromto $1 $6 }
/*| SUPER DOT type_arguments IDENTIFIER LPAREN argument_list_opt RPAREN*/
| name DOT SUPER DOT IDENTIFIER LPAREN argument_list_opt RPAREN
    { Method_call (Named_super (fst $1,$5), $7), fromto $1 $8 }
/*| name DOT SUPER DOT type_arguments IDENTIFIER LPAREN argument_list_opt RPAREN*/

array_access:
| name LBRACK expression RBRACK 
    { Binary (Array_access, (Variable (fst $1), snd $1), $3), fromto $1 $4 }
| primary_no_new_array LBRACK expression RBRACK 
    { Binary (Array_access, $1, $3), fromto $1 $4 }
| array_creation_init LBRACK expression RBRACK 
    { Binary (Array_access, $1, $3), fromto $1 $4 }

postfix_expression:
| primary                   { $1 }
| name                      { Variable(fst $1), snd $1 }
| postincrement_expression  { $1 }
| postdecrement_expression  { $1 }

postincrement_expression:
| postfix_expression PLUSPLUS       { Unary(Post_incr, $1), fromto $1 $2 }

postdecrement_expression:
| postfix_expression MINUSMINUS     { Unary(Post_decr, $1), fromto $1 $2 }

unary_expression:
| preincrement_expression           { $1 }
| predecrement_expression           { $1 }
| PLUS unary_expression             { fst $2, fromto $1 $2 }
| MINUS unary_expression            { Unary (Minus, $2), fromto $1 $2 }
| unary_expression_not_plus_minus   { $1 }
    
preincrement_expression:
| PLUSPLUS unary_expression         { Unary (Pre_incr, $2), fromto $1 $2 }

predecrement_expression:
| MINUSMINUS unary_expression       { Unary (Pre_decr, $2), fromto $1 $2 }

unary_expression_not_plus_minus:
| postfix_expression      { $1 }
| COMP unary_expression   { Unary (Complement, $2), fromto $1 $2 }
| NOT unary_expression    { Unary (Not, $2), fromto $1 $2 }
| cast_expression         { $1 }
	    
cast_expression:
| LPAREN primitive_type dims_opt RPAREN unary_expression
    { Unary (Cast (lift_array (fst $2) (fst $3)), $5), fromto $1 $5 }
| LPAREN name RPAREN unary_expression_not_plus_minus
    { Unary (Cast (Class_type (Simple_class_type (fst $2))), $4), fromto $1 $4 }
| LPAREN name dims RPAREN unary_expression_not_plus_minus
    { Unary (Cast (lift_array (Class_type (Simple_class_type (fst $2))) 
                     (fst $3) ), $5),
      fromto $1 $5 }
| LPAREN name LT type_argument_list_1 dims_opt RPAREN
    unary_expression_not_plus_minus
    { Unary (Cast (Class_type Unsupported_class_type), $7),
      fromto $1 $7 }
| LPAREN name LT type_argument_list_1 DOT
    class_or_interface_type dims_opt RPAREN
    unary_expression_not_plus_minus
    { Unary (Cast (Class_type Unsupported_class_type), $9),
      fromto $1 $9 }

multiplicative_expression:
| unary_expression {$1}
| multiplicative_expression MULT unary_expression
    { Binary(Mult, $1, $3), fromto $1 $3 }
| multiplicative_expression DIV unary_expression
    { Binary(Div, $1, $3), fromto $1 $3 }
| multiplicative_expression MOD unary_expression
    { Binary(Modulo, $1, $3), fromto $1 $3 }
    
additive_expression:
| multiplicative_expression {$1}
| additive_expression PLUS multiplicative_expression
    { Binary(Add, $1, $3), fromto $1 $3 }
| additive_expression MINUS multiplicative_expression
    { Binary(Sub, $1, $3), fromto $1 $3 }
    
shift_expression:
| additive_expression {$1}
| shift_expression LSHIFT additive_expression
    { Binary(Lshift, $1, $3), fromto $1 $3 }
| shift_expression RSHIFT additive_expression
    { Binary(Rshift, $1, $3), fromto $1 $3 }
| shift_expression URSHIFT additive_expression
    { Binary(Urshift, $1, $3), fromto $1 $3 }
    
relational_expression:
| shift_expression {$1}
| relational_expression LT shift_expression
    { Binary(Lesser_than, $1, $3), fromto $1 $3 }
| relational_expression GT shift_expression
    { Binary(Greater_than, $1, $3), fromto $1 $3 }
| relational_expression LTEQ shift_expression
    { Binary(Lesser_equal, $1, $3), fromto $1 $3 }
| relational_expression GTEQ shift_expression
    { Binary(Greater_equal, $1, $3), fromto $1 $3 }

instanceof_expression:
| relational_expression {$1}
| instanceof_expression INSTANCEOF reference_type
    { Instanceof($1, fst $3), fromto $1 $3 }

equality_expression:
| instanceof_expression {$1}
| equality_expression EQEQ instanceof_expression
    { Binary(Equal, $1, $3), fromto $1 $3 }
| equality_expression NOTEQ instanceof_expression
    { Unary(Not,(Binary(Equal, $1, $3), fromto $1 $3)), fromto $1 $3 }

and_expression:
| equality_expression {$1}
| and_expression AND equality_expression
    { Binary(Bitwise_and, $1, $3), fromto $1 $3 }

exclusive_or_expression:
| and_expression {$1}
| exclusive_or_expression XOR and_expression
    { Binary(Xor, $1, $3), fromto $1 $3 }

inclusive_or_expression:
| exclusive_or_expression {$1}
| inclusive_or_expression OR exclusive_or_expression
    { Binary(Bitwise_or, $1, $3), fromto $1 $3 }
    
conditional_and_expression:
| inclusive_or_expression {$1}
| conditional_and_expression ANDAND inclusive_or_expression
    { Binary(And, $1, $3), fromto $1 $3 }
    
conditional_or_expression:
| conditional_and_expression {$1}
| conditional_or_expression OROR conditional_and_expression
    { Binary(Or, $1, $3), fromto $1 $3 }
    
conditional_expression:
| conditional_or_expression {$1}
| conditional_or_expression QUESTION expression 
    COLON conditional_expression
    { Conditional($1,($3,$5)), fromto $1 $5 }
    
assignment_expression:
| conditional_expression {$1}
| assignment {Assignment $1, let (e1,_,e2)=$1 in fromto e1 e2}
    
/* semantic check necessary here to ensure a valid left-hand side.
 allowing a parenthesized variable here on the lhs was introduced in
 JLS 2; thanks to Eric Blake for pointing this out. */
assignment:
| postfix_expression assignment_operator assignment_expression
    { $1, $2, $3 }

assignment_operator:
| EQ {None}
| MULTEQ {Some Mult}
| DIVEQ {Some Div}
| MODEQ {Some Modulo}
| PLUSEQ {Some Add}
| MINUSEQ {Some Sub}
| LSHIFTEQ {Some Lshift}
| RSHIFTEQ {Some Rshift}
| URSHIFTEQ {Some Urshift}
| ANDEQ {Some And}
| XOREQ {Some Xor}
| OREQ {Some Or}

expression_opt:
| { None }
| expression {Some $1}

expression:
| assignment_expression {$1}
    
/* note that this constraint must be enforced during semantic checking
 'constant_expression' should include enumerated constants. */
constant_expression:
| expression {$1}

/* JLS-14 productions, not supported. */
type_parameters_opt:
| type_parameters 
    { failwith "Type parameters not supported by the analyzer" } 
| {()};
type_parameters:
| LT type_parameter_list_1 {}

type_parameter_list:
| type_parameter_list COMMA type_parameter {}
| type_parameter {}

type_parameter_list_1:
| type_parameter_1 {}
| type_parameter_list COMMA type_parameter_1 {}

type_parameter:
| type_variable type_bound_opt {}

type_parameter_1:
| type_variable GT {}
| type_variable type_bound_1 {}

type_bound_opt: type_bound {} | {};
type_bound:
| EXTENDS reference_type additional_bound_list_opt {}

type_bound_1:
| EXTENDS reference_type_1 {}
| EXTENDS reference_type additional_bound_list_1 {}

additional_bound_list_opt:
|  additional_bound_list {}
| {};
additional_bound_list:
| additional_bound additional_bound_list {}
| additional_bound {}

additional_bound_list_1:
| additional_bound additional_bound_list_1 {}
| additional_bound_1 {}

additional_bound:
| AND interface_type {}

additional_bound_1:
| AND reference_type_1 {}

/* standard expression grammar, without names */
postfix_expression_nn:
| primary {$1}
| postincrement_expression {$1}
| postdecrement_expression {$1}

unary_expression_nn:
| preincrement_expression             { $1 }
| predecrement_expression             { $1 }
| PLUS unary_expression               { fst $2, fromto $1 $2 }
| MINUS unary_expression              { Unary (Minus, $2), fromto $1 $2 }
| unary_expression_not_plus_minus_nn  { $1 }

unary_expression_not_plus_minus_nn:
| postfix_expression_nn      { $1 }
| COMP unary_expression      { Unary (Complement, $2), fromto $1 $2 }
| NOT unary_expression       { Unary (Not, $2), fromto $1 $2 }
| cast_expression            { $1 }

multiplicative_expression_nn:
| unary_expression_nn
    { $1 }
| name MULT unary_expression
    { Binary (Mult, (Variable (fst $1), snd $1), $3), fromto $1 $3 }
| multiplicative_expression_nn MULT unary_expression
    { Binary (Mult, $1, $3), fromto $1 $3 }
| name                         DIV unary_expression
    { Binary (Div, (Variable (fst $1), snd $1), $3), fromto $1 $3 }
| multiplicative_expression_nn DIV unary_expression
    { Binary (Div, $1, $3), fromto $1 $3 }
| name                         MOD unary_expression
    { Binary (Modulo, (Variable (fst $1), snd $1), $3), fromto $1 $3 }
| multiplicative_expression_nn MOD unary_expression
    { Binary (Modulo, $1, $3), fromto $1 $3 }
    
additive_expression_nn:
| multiplicative_expression_nn {$1}
| name                   PLUS multiplicative_expression
    { Binary(Add, (Variable(fst $1), snd $1), $3),
      fromto $1 $3 }
| additive_expression_nn PLUS multiplicative_expression
    { Binary(Add, $1, $3),
      fromto $1 $3 }
| name                   MINUS multiplicative_expression
    { Binary(Sub, (Variable(fst $1), snd $1), $3),
      fromto $1 $3 }
| additive_expression_nn MINUS multiplicative_expression
    { Binary(Sub, $1, $3),
      fromto $1 $3 }
    
shift_expression_nn:
| additive_expression_nn {$1}
| name                LSHIFT additive_expression
    { Binary(Lshift, (Variable(fst $1), snd $1), $3),
      fromto $1 $3 }
| shift_expression_nn LSHIFT additive_expression
    { Binary(Lshift, $1, $3),
      fromto $1 $3 }
| name                RSHIFT additive_expression
    { Binary(Rshift, (Variable(fst $1), snd $1), $3),
      fromto $1 $3 }
| shift_expression_nn RSHIFT additive_expression
    { Binary(Rshift, $1, $3),
      fromto $1 $3 }
| name                URSHIFT additive_expression
    { Binary(Urshift, (Variable(fst $1), snd $1), $3),
      fromto $1 $3 }
| shift_expression_nn URSHIFT additive_expression
    { Binary(Urshift, $1, $3),
      fromto $1 $3 }

relational_expression_nn:
| shift_expression_nn {$1}
    /* note that we've tweaked the productions for LT/GT to disallow
	a<b<c as a valid expression.  This avoids ambiguity with
parameterized types in casts. */
| name                LT shift_expression
    { Binary(Lesser_than, (Variable(fst $1), snd $1), $3),
      fromto $1 $3 }
| shift_expression_nn LT shift_expression
    { Binary(Lesser_than, $1, $3),
      fromto $1 $3 }
| name                GT shift_expression
    { Binary(Greater_than, (Variable(fst $1), snd $1), $3),
      fromto $1 $3 }
| shift_expression_nn GT shift_expression
    { Binary(Greater_than, $1, $3),
      fromto $1 $3 }
| name                     LTEQ shift_expression
    { Binary(Lesser_equal, (Variable(fst $1), snd $1), $3),
      fromto $1 $3 }
| relational_expression_nn LTEQ shift_expression
    { Binary(Lesser_equal, $1, $3),
      fromto $1 $3 }
| name                     GTEQ shift_expression
    { Binary(Greater_equal, (Variable(fst $1), snd $1), $3),
      fromto $1 $3 }
| relational_expression_nn GTEQ shift_expression
    { Binary(Greater_equal, $1, $3),
      fromto $1 $3 }
    
instanceof_expression_nn:
| relational_expression_nn {$1}
| name                     INSTANCEOF reference_type
    { Instanceof((Variable(fst $1), snd $1), fst $3),
      fromto $1 $3 }
| instanceof_expression_nn INSTANCEOF reference_type
    { Instanceof($1, fst $3),
      fromto $1 $3 }

equality_expression_nn:
| instanceof_expression_nn {$1}
| name EQEQ instanceof_expression
    { Binary(Equal, (Variable(fst $1), snd $1), $3), fromto $1 $3 }
| equality_expression_nn EQEQ instanceof_expression
    { Binary(Equal, $1, $3), fromto $1 $3 }
| name NOTEQ instanceof_expression
    { Unary(Not,(Binary(Equal, (Variable(fst $1), snd $1), $3), fromto $1 $3)),
      fromto $1 $3 }
| equality_expression_nn NOTEQ instanceof_expression
    { Unary(Not,(Binary(Equal, $1, $3), fromto $1 $3)),
      fromto $1 $3 }

and_expression_nn:
| equality_expression_nn {$1}
| name AND equality_expression
    { Binary(Bitwise_and, (Variable(fst $1), snd $1), $3),
      fromto $1 $3 }
| and_expression_nn AND equality_expression
    { Binary(Bitwise_and, $1, $3),
      fromto $1 $3 }

exclusive_or_expression_nn:
| and_expression_nn {$1}
| name XOR and_expression
    { Binary(Xor, (Variable(fst $1), snd $1), $3),
      fromto $1 $3 }
| exclusive_or_expression_nn XOR and_expression
    { Binary(Xor, $1, $3),
      fromto $1 $3 }
    
inclusive_or_expression_nn:
| exclusive_or_expression_nn {$1}
| name OR exclusive_or_expression
    { Binary(Bitwise_or, (Variable(fst $1), snd $1), $3),
      fromto $1 $3 }
| inclusive_or_expression_nn OR exclusive_or_expression
    { Binary(Bitwise_or, $1, $3),
      fromto $1 $3 }

conditional_and_expression_nn:
| inclusive_or_expression_nn {$1}
| name ANDAND inclusive_or_expression
    { Binary(And, (Variable(fst $1), snd $1), $3),
      fromto $1 $3 }
| conditional_and_expression_nn ANDAND inclusive_or_expression
    { Binary(And, $1, $3),
      fromto $1 $3 }

conditional_or_expression_nn:
| conditional_and_expression_nn {$1}
| name OROR conditional_and_expression
    { Binary(Or, (Variable(fst $1), snd $1), $3),
      fromto $1 $3 }
| conditional_or_expression_nn OROR conditional_and_expression
    { Binary(Or, $1, $3), fromto $1 $3 }

conditional_expression_nn:
| conditional_or_expression_nn {$1}
| name QUESTION expression COLON conditional_expression
    { Conditional((Variable(fst $1), snd $1),($3,$5)),
     fromto $1 $5 }
| conditional_or_expression_nn QUESTION expression COLON conditional_expression
    { Conditional($1,($3,$5)), fromto $1 $5 }

assignment_expression_nn:
| conditional_expression_nn {$1}
| assignment {Assignment $1, let (e1,_,e2)=$1 in fromto e1 e2}

expression_nn:
| assignment_expression_nn {$1}


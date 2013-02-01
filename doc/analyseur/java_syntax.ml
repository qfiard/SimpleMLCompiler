(* java_syntax.ml *)
(* la syntaxe abstraite de Java 1.5 *)
open Localizing

type identifier =
    string * extent

type name =
  | Simple_name of identifier
  | Qualified_name of name * string

type import_type =
  | Single_type_import
  | Type_import_on_demand

type import =
    { import_type:    import_type;
      import_static:  bool;
      import_name:    name }

type modifier =
  | Public | Protected | Private | Static | Abstract | Final
  | Native | Synchronized | Transient | Volatile | Strictfp

type class_type =
  | Simple_class_type of name
  | Unsupported_class_type

type int_type =
  | Byte | Short | Int | Long | Char

type float_type =
  | Float | Double

type var_type =
  | Boolean
  | Integer of int_type
  | Floating_point of float_type
  | Class_type of class_type
  | Array_type of var_type
  | Void

type unary_op =
  | Not
  | Cast of var_type
  | Complement
  | Pre_decr
  | Post_decr
  | Pre_incr
  | Post_incr
  | Minus

type binary_op =
  (* Logic *)
  | Or | And | Xor
  (* Bitwise operations *)
  | Bitwise_and | Bitwise_or | Lshift | Rshift | Urshift
  (* Comparison *)
  | Equal
  | Lesser_than | Greater_than
  | Lesser_equal | Greater_equal
  (* Arithmetic *)
  | Add | Sub | Mult | Div | Modulo
  (* Arrays *)
  | Array_access

type formal_parameter =
    { param_type:         var_type;
      param_name:         identifier;
      param_is_final:     bool;
      param_is_ellipsis:  bool }
      
type type_args = unit (* not implemented yet *)

type instance_creation =
    { instance_type:      class_type;
      instance_type_args: type_args list;
      instance_args:      expr_e list;
      instance_body:      class_body_decl list }

and array_creation =
    { array_type:       var_type;
      array_dimensions: expr_e list;
      array_init:       var_init list }

and field_obj =
  | Var_expr of expr_e 
  | Super

and method_name =
  | Named_method of name
  | Named_super of name * identifier
  | Expr_method of expr_e * identifier
  | Super_method of identifier

and expression =
  | Assignment of assignment
  | Conditional of expr_e * (expr_e * expr_e)
  | Binary of binary_op * expr_e * expr_e
  | Unary of unary_op * expr_e
  | Variable of name
  | Integer_constant of Int64.t
  | Float_constant of float
  | Bool_constant of bool
  | Null
  | Char_constant of string
  | String_constant of string
  | This
  | New of instance_creation
  | Field_access of  field_obj * string
  | Method_call of method_name * (expr_e list)
  | New_array of array_creation
  | Instanceof of expr_e * var_type
	
and expr_e = expression * extent

and assignment = expr_e * (binary_op option) * expr_e

and var_init =
  | Expr_init of expr_e
  | Array_init of var_init list

and variable =
    { var_modifiers:    modifier list;
      var_type:         var_type;
      var_name:         identifier;
      var_initializer:  var_init option }

and method_declaration =
    { method_modifiers:  modifier list;
      method_type:       var_type;
      method_throws:     class_type list;
      method_name:       identifier;
      method_parameters: formal_parameter list;
      method_body:       block }

and constructor_declaration =
    { constructor_modifiers:  modifier list;
      constructor_throws:     class_type list;
      constructor_name:       name;
      constructor_parameters: formal_parameter list;
      constructor_invocation: (constructor_invocation 
				 * (expr_e list)) option;
      constructor_body:       block }
      
and constructor_invocation =
  | This_constructor
  | Super_constructor
  | Super_const_of_expr of expr_e
  | Super_const_of_name of name
	
and for_init =
  | For_loc_var of variable list
  | For_expressions of expr_e list

and for_statement =
    { for_init:   for_init;
      for_test:   expr_e option;
      for_update: expr_e list;
      for_body:   statement_e }

and foreach_statement =
    { foreach_var:  variable;
      foreach_val:  expr_e;
      foreach_body: statement_e }

and switch_label =
  | Case of expr_e
  | Default

and switch_block = switch_label list * block

and try_statement =
    { try_block:  statement_e list;
      try_catch:  (formal_parameter * block) list;
      try_finaly: (statement_e list) option }

and statement =
  | Local_var of variable 
  | Type_decl of type_decl 
  | Labeled of identifier * statement_e
  | If_then of expr_e * statement_e
  | If_then_else of expr_e * statement_e * statement_e
  | While of expr_e * statement_e
  | For of for_statement
  | Expression of expr_e
  | Foreach of foreach_statement
  | Block_statement of block
  | Empty_statement
  | Switch of expr_e * (switch_block list)
  | Do_while of statement_e * expr_e
  | Break of identifier option
  | Continue of identifier option
  | Return of expr_e option
  | Throw of expr_e
  | Synchronized_statement of expr_e * block
  | Try of try_statement
  | Assert of expr_e * expr_e option

and statement_e = statement * extent

and block = statement_e list

and class_member =
  | Field_decl of variable
  | Method_decl of method_declaration
  | Class_decl of class_decl
  | Enum_decl of enum_declaration
  | Interface_decl of class_decl

and class_body_decl =
  | Class_member of class_member
  | Static_init of statement_e list
  | Constructor_decl of constructor_declaration
  | Block of statement_e list

and class_decl =
    { class_modifiers:   modifier list;
      class_name:        identifier;
      class_inherits:    class_type option;
      class_interfaces:  class_type list;
      class_body:        class_body_decl list }

and enum_declaration =
    { enum_modifiers:   modifier list;
      enum_name:        string;
      enum_interfaces:  class_type list;
      enum_body:        (enum_constant list)*(class_body_decl list)}

and enum_constant = 
    { enum_constant_name: string;
      enum_constant_args: expr_e list;
      enum_constant_body: class_body_decl list }

and type_decl =
  | Class_declaration of class_decl
  | Enum_declaration of enum_declaration
  | Interface_declaration of class_decl

type java_prog =
    { package:    name option;
      import:     import list;
      type_decls: type_decl list }
      

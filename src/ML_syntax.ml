type op = Plus | Minus | Times | Div | And | Or

type var = string

type const = Int of int | Bool of bool | Id of var

type expression =
    (* Local variable definition *)
      Local of     var    (* Variable name *)
                *  expression       (* Expression to assign *)
                *  expression       (* Scope *)
    (* Definition of a local recursive function with one argument *)
    | RecFun of    var           (* Function name *)
                *  var           (* Argument identifier *)
                *  expression       (* Fonction body *)
                *  expression       (* Scope *)
    (* Definition of an anonymous function with one argument *)
    | Fun of    var              (* Argument identifier *)
             *  expression          (* Fonction body *)
    (* Functional evaluation *)
    | Eval of    expression         (* Function *)
             *   expression         (* Argument *)
    (* Binary operation *)
    | Binary of    op        (* Operator *)
                 * expression       (* First operand *)
                 * expression       (* Second operand *)
    (* Conditional *)
    | If of        expression       (* Condition *)
                 * expression       (* Executed if condition is true *)
                 * expression       (* Executed if condition is false *)
    | Const of const         (* Constant *)

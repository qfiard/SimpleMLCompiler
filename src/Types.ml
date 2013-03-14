type op = Plus | Minus | Times | Div | And | Or

let is_int_op = function
    | And -> false
    | Or -> false
    | _ -> true

type dbindex = int

(* Definition of the types that expressions are allowed to bear *)
(* The index for generic types is used to uniquely identify a generic type so as to determine *)
(* if the types when restrained must be equal or not *)
type exprType = IntType | BoolType | GenericType of int | FunType of (exprType*exprType)
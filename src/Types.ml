open Printf
open Char

type op = Plus | Minus | Times | Div | And | Or | Eq | Neq | Gt | Ge | Lt | Le

let is_int_op = function
    | And -> false
    | Or -> false
    | _ -> true

let is_bool_op = function
    | Eq -> true
    | Neq -> true
    | op -> not(is_int_op op)

let is_comparator_op = function
    | Eq -> true
    | Neq -> true
    | Lt -> true
    | Le -> true
    | Gt -> true
    | Ge -> true
    | _ -> false

let syscalls = ["print";"print_newline"]

type dbindex = int

(* Definition of the types that expressions are allowed to bear *)
(* The index for generic types is used to uniquely identify a generic type so as to determine *)
(* if the types when restrained must be equal or not *)
type exprType = IntType | BoolType | UnitType | GenericType of int | FunType of (exprType*exprType)

let rec are_types_equal t1 t2 = match t1,t2 with
    | IntType,IntType -> true
    | BoolType,BoolType -> true
    | GenericType i,GenericType j -> i==j
    | FunType(t1,t2),FunType(t3,t4) -> (are_types_equal t1 t3) && (are_types_equal t2 t4)
    | _ -> false

let outputType t =
    let rec outputTypeAux = function
    | IntType -> printf "int"
    | BoolType -> printf "bool"
    | UnitType -> printf "unit"
    | GenericType i ->
        printf "'";
        print_char(chr ((code 'a')+i))
    | FunType(FunType(t1,t2),FunType(t3,t4)) ->
        printf "(";
        outputTypeAux(FunType(t1,t2));
        printf ") -> (";
        outputTypeAux(FunType(t3,t4));
        printf ")";
    | FunType(FunType(t1,t2),t3) ->
        printf "(";
        outputTypeAux(FunType(t1,t2));
        printf ") -> ";
        outputTypeAux(t3);
    | FunType(t1,FunType(t3,t4)) ->
        outputTypeAux(t1);
        printf " -> (";
        outputTypeAux(FunType(t3,t4));
        printf ")";
    | FunType(t1,t2) ->
        outputTypeAux(t1);
        printf " -> ";
        outputTypeAux(t2);
    
    in
    printf "Type : ";
    outputTypeAux t;
    printf "\n"
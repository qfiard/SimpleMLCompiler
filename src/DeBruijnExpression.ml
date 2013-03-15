open Types
open Printf

type var = dbindex

type const = Int of int | Bool of bool | Var of var

type expression =
    (* Local variable definition *)
      Local of     expression       (* Expression to assign *)
                *  expression       (* Scope *)
    (* Definition of a local recursive function with one argument *)
    | RecFun of    expression       (* Fonction body *)
                *  expression       (* Scope *)
    (* Definition of an anonymous function with one argument *)
    | Fun of     expression          (* Fonction body *)
    (* Functional evaluation *)
    | Eval of    expression         (* Function *)
             *   expression         (* Argument *)
    (* Side effect *)
    | SideEffect of    expression         (* Side effect *)
                   *   expression         (* Result *)
    (* Binary operation *)
    | Binary of    op               (* Operator *)
                 * expression       (* First operand *)
                 * expression       (* Second operand *)
    (* Conditional *)
    | If of        expression       (* Condition *)
                 * expression       (* Executed if condition is true *)
                 * expression       (* Executed if condition is false *)
    | Const of const                (* Constant *)

let print_dbe_const = function
    | Int c -> printf "Integer %d" c
    | Bool true -> printf "Boolean true"
    | Bool false -> printf "Boolean false"
    | Var v -> printf "Variable %d" v
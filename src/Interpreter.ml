open Types;;
open DeBruijnExpression;;
open Pervasives;;
open Utilities;;

type value = ConstVal of const | FunVal of Expression.expression
type state = value list

let rec constValue state = function
    | ConstVal(Int i) -> ConstVal(Int i)
    | ConstVal(Bool b) -> ConstVal(Bool b)
    | ConstVal(Var i) when List.length state > i -> constValue state (List.nth state i)
    | ConstVal(Var i) -> raise(failwith "Variable is not defined")
    | _ as c -> c

let rec constValueAsPrimary state = function
    | ConstVal(Int i) -> Int i
    | ConstVal(Bool b) -> Bool b
    | ConstVal(Var i) when List.length state > i -> constValueAsPrimary state (List.nth state i)
    | ConstVal(Var i) -> raise(failwith "Variable is not defined")
    | _ -> raise(failwith "Tried to gather a primary value from a function definition")

let interpretBinOpWithState state op v1 v2 =
    let c1 = constValueAsPrimary state v1
    and c2 = constValueAsPrimary state v2 in
    match c1,c2 with
    | Int i, Int j when is_comparator_op op -> ConstVal(Bool (interpretIntBinCompareOp op i j))
    | Int i, Int j -> ConstVal(Int (interpretIntBinOp op i j))
    | Bool i, Bool j -> ConstVal(Bool (interpretBoolBinOp op i j))
    | _ -> raise(failwith "Non homogeneous operation")

(* interpret_DBE is able to interpret a program represented as an expression *)
(* of type DeBruijnExpression.expression *)
let interpret_DBE (p:expression) : value =
    let rec interpretWithState state = function
        | Local(e,scope) ->
            let value = interpretWithState state e in
            interpretWithState (value::state) scope
        | RecFun(body,scope) ->
            let f_value = FunVal (Expression.DBE body) in
            interpretWithState (f_value::state) scope
        | Fun(body) ->
            FunVal (Expression.DBE body)
        | Eval(e1,e2) ->
            let f = interpretWithState state e1 in
            let value = interpretWithState state e2 in
            begin
            match f with
            | FunVal(Expression.DBE body) as f -> interpretWithState (f::value::state) body
            | _ -> raise(failwith "Cannot evaluate an element that is not a function")
            end
        | Binary(op,e1,e2) ->
            let val1 = interpretWithState state e1
            and val2 = interpretWithState state e2 in
            interpretBinOpWithState state op val1 val2
        | If(cond,e1,e2) ->
            let cond_value = interpretWithState state cond in
            begin
            match cond_value with
            | ConstVal(Bool true) -> interpretWithState state e1
            | ConstVal(Bool false) -> interpretWithState state e2
            | _ -> raise(failwith "Condition must be boolean")
            end
        | Const c -> constValue state (ConstVal c)
    in
    interpretWithState [] p;;

(* Only DBE expression interpretation is supported for now *)
let interpret = function
    | Expression.DBE e -> interpret_DBE e
    | _ -> raise(Failure "Not implemented")
    
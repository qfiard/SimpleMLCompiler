open Types;;
open DeBruijnExpression;;
open Pervasives;;

type value = ConstVal of const | FunVal of expression
type state = value list

let rec constValue state = function
    | ConstVal(Int i) -> Int i
    | ConstVal(Bool b) -> Bool b
    | ConstVal(Var i) when List.length state > i -> constValue state (List.nth state i)
    | ConstVal(Var i) -> raise(failwith "Variable is not defined")
    | _ -> raise(failwith "Tried to gather a value from a functio definition")

let interpretIntBinOp op i j = match op with
    | Plus -> i+j
    | Minus -> i-j
    | Times -> i*j
    | Div when j!=0 -> i/j
    | Div -> raise(failwith "Division by 0")
    | _ -> raise(failwith "Invalid operation on integers")

let interpretBoolBinOp op i j = match op with
    | And -> i && j
    | Or -> i || j
    | _ -> raise(failwith "Invalid operation on booleans")

let interpretBinOpWithState state op v1 v2 =
    let c1 = constValue state v1
    and c2 = constValue state v2 in
    match c1,c2 with
    | Int i, Int j -> ConstVal(Int (interpretIntBinOp op i j))
    | Bool i, Bool j -> ConstVal(Bool (interpretBoolBinOp op i j))
    | _ -> raise(failwith "Non homogeneous operation")

let interpret (p:expression) : value =
    let rec interpretWithState state = function
        | Local(e,scope) ->
            let value = interpretWithState state e in
            interpretWithState (value::state) scope
        | RecFun(body,scope) ->
            let f_value = FunVal body in
            interpretWithState (f_value::state) scope
        | Fun(body) ->
            FunVal body
        | Eval(e1,e2) ->
            let value = interpretWithState state e2 in
            interpretWithState (value::state) e1
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
        | Const c -> ConstVal c
    in
    interpretWithState [] p;;
    
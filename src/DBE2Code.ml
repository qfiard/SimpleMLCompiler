open Types
open List

open AbstractMachine

let rec dbe2code =  function
    | DeBruijnExpression.Local(e,scope) ->
        let value = dbe2code e in
        let scope = dbe2code scope in
        value@(Let::scope)
    | DeBruijnExpression.RecFun(body,scope) ->
        let body = dbe2code body in
        let body = (Let::(Access 0)::body)@(Endlet::Return::[]) in
        let scope = dbe2code scope in
        (Cur body)::Let::(Access 0)::scope;
    | DeBruijnExpression.Fun(body) ->
        let body = dbe2code body in
        (Cur body)::[]
    | DeBruijnExpression.Eval(e1,e2) ->
        let e1 = dbe2code e1 in
        let e2 = dbe2code e2 in
        e2@e1@[Apply]
    | DeBruijnExpression.SideEffect(e1,e2) -> dbe2code e2 (* We have to skip the side effect as no side effect is allowed by the abstract machine *)
    | DeBruijnExpression.Binary(op,e1,e2) ->
        let e1 = dbe2code e1 in
        let e2 = dbe2code e2 in
        e2@e1@[Op op]
    | DeBruijnExpression.If(cond,e1,e2) ->
        let cond = dbe2code cond in
        let e1 = dbe2code e1 in
        let e2 = dbe2code e2 in
        let n1 = length e1 in
        let n2 = length e2 in
        cond@((Branchneg (n1+1))::e1)@((Branch n2)::e2)
    | DeBruijnExpression.Const(DeBruijnExpression.Int i) -> [Push(Int i)]
    | DeBruijnExpression.Const(DeBruijnExpression.Bool b) -> [Push(Bool b)]
    | DeBruijnExpression.Const(DeBruijnExpression.Var i) -> [Access i]
    | DeBruijnExpression.Const(DeBruijnExpression.Unit) -> [Push Unit]
    | DeBruijnExpression.Const(DeBruijnExpression.SysCall v) -> [Cur [Push Unit]]
    | _ -> raise(failwith "Unknown pattern")
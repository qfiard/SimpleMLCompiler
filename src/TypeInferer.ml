open Types
open DeBruijnExpression
open List

(* inferTypeWithEnv takes an environment, a next *)
(* available index for a generic type and an expression as argument, *)
(* and returns the type of the expression and the updated environment *)
(* given type constraints due to operations *)

let notWellTyped() = raise(failwith "Expression is not well-typed")

let computeTypeForEval t1 t2 = match t1,t2 with
    | FunType(GenericType i,t2),_ -> t2
    | FunType(t1,t2),t3 when t1==t3 -> t2
    | _ -> notWellTyped()

let rec replaceGenericTypeInType i toReplace = function
    | GenericType i -> toReplace
    | FunType(t1,t2) -> FunType(replaceGenericTypeInType i toReplace t1, replaceGenericTypeInType i toReplace t2)
    | t -> t

let rec replaceGenericTypeInEnv i toReplace = function
    | [] -> []
    | x::s -> (replaceGenericTypeInType i toReplace x)::(replaceGenericTypeInEnv i toReplace s)

let rec mergeTypes t1 t2 env1 env2 = match t1,t2 with
    | GenericType i,t2 when t1!=t2 ->
        let env1 = replaceGenericTypeInEnv i t2 env1 in
        t2,env1,env2
    | t1,GenericType i when t1!=t2 ->
        let env2 = replaceGenericTypeInEnv i t1 env2 in
        t1,env1,env2
    | FunType(t11,t12),FunType(t21,t22) ->
        let t1,env11,env21 = mergeTypes t11 t21 env1 env2 in
        let t2,env12,env22 = mergeTypes t11 t21 env1 env2 in
        FunType(t1,t2),mergeEnv env11 env12,mergeEnv env21 env22
    | t1,t2 when t1 == t2 -> t1,env1,env2
    | _,_ -> notWellTyped()

and mergeEnv env1 env2 = 
    let rec mergeEnvAux env1 env2 env1bis env2bis = match env1bis,env2bis with
        | [],_ -> []
        | _,[] -> []
        | t1::s1,t2::s2 when t1 != t2 ->
            let t,env1New,env2New = mergeTypes t1 t2 env1 env2 in
            mergeEnv env1New env2New
        | t1::s1,t2::s2 -> t1::(mergeEnvAux env1 env2 s1 s2) in
    mergeEnvAux env1 env2 env1 env2;;

let updateEnvForOperation op type1 type2 env = match op,type1,type2 with
    | op,GenericType i,GenericType j when is_int_op op ->
        let newEnv1 = replaceGenericTypeInEnv i IntType env in
        let newEnv2 = replaceGenericTypeInEnv j IntType newEnv1 in
        IntType,newEnv2
    | op,GenericType i,IntType when is_int_op op ->
        let newEnv1 = replaceGenericTypeInEnv i IntType env in
        IntType,newEnv1
    | op,IntType,GenericType i when is_int_op op ->
        let newEnv1 = replaceGenericTypeInEnv i IntType env in
        IntType,newEnv1
    | op,IntType,IntType when is_int_op op -> IntType,env
    | op,_,_ when is_int_op op -> notWellTyped()
    | op,GenericType i,GenericType j ->
        let newEnv1 = replaceGenericTypeInEnv i BoolType env in
        let newEnv2 = replaceGenericTypeInEnv j BoolType newEnv1 in
        BoolType,newEnv2
    | op,GenericType i,BoolType ->
        let newEnv1 = replaceGenericTypeInEnv i BoolType env in
        BoolType,newEnv1
    | op,BoolType,GenericType i ->
        let newEnv1 = replaceGenericTypeInEnv i BoolType env in
        BoolType,newEnv1
    | op,BoolType,BoolType -> BoolType,env
    | _ -> notWellTyped()

let rec inferTypeWithEnv env index = function
    | Local(e,scope) ->
        let varType,newEnv = inferTypeWithEnv env index e in
        inferTypeWithEnv (varType::newEnv) index scope
    | RecFun(body,scope) ->
        let resType,newEnv = inferTypeWithEnv ((FunType(GenericType index,GenericType (index+1)))::(GenericType index)::env) (index+2) body in
        inferTypeWithEnv ((hd newEnv)::env) (index+2) scope
    | Fun(body) ->
        let resType,newEnv = inferTypeWithEnv ((GenericType index)::env) (index+1) body in
        FunType(hd newEnv,resType),newEnv
    | Eval(e1,e2) ->
        let type1,newEnv1 = inferTypeWithEnv env index e1 in
        let type2,newEnv2 = inferTypeWithEnv env index e2 in
        let newEnv = mergeEnv newEnv1 newEnv2 in
        computeTypeForEval type1 type2,newEnv
    | Binary(op,e1,e2) ->
        let type1,newEnv1 = inferTypeWithEnv env index e1 in
        let type2,newEnv2 = inferTypeWithEnv env index e2 in
        let newEnv1 = mergeEnv newEnv1 newEnv2 in
        let t,newEnv2 = updateEnvForOperation op type1 type2 newEnv1 in
        t,newEnv2
    | If(cond,e1,e2) ->
        let typeCond,newEnv1 = inferTypeWithEnv env index cond in
        let type1,newEnv2 = inferTypeWithEnv env index e1 in
        let type2,newEnv3 = inferTypeWithEnv env index e2 in
        let newEnv4 = mergeEnv newEnv1 newEnv2 in
        let newEnv = mergeEnv newEnv3 newEnv4 in
        begin
        match typeCond,type1,type2 with
            | x,_,_ when x != BoolType -> raise(failwith "Condition must be boolean")
            | _,GenericType i, t2 when type1!=t2 ->
                let newEnv = replaceGenericTypeInEnv i t2 newEnv in
                t2,newEnv
            | _,t1,GenericType i when t1!=type2 ->
                let newEnv = replaceGenericTypeInEnv i t1 newEnv in
                t1,newEnv
            | _,t1,t2 when t1==t2 ->
                t1,newEnv
            | _ -> notWellTyped()
        end
    | Const(Int i) -> IntType,env
    | Const(Bool b) -> BoolType,env
    | Const(Var i) when i> (length env)-1 -> raise(failwith "Undefined variable encountered")
    | Const(Var i) -> nth env i,env
    | _ -> raise(failwith "Unknown pattern")

let inferType = function
    | Expression.DBE e ->
        let exprType,env = inferTypeWithEnv [] 0 e in exprType
    | _ -> raise(failwith "Not implemented")
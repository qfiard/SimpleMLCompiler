open ML_syntax;;

module StringMap = Map.Make(String);;

let rec updateDeBruijnIndexesWithVarMap map ast =
    let f = updateDeBruijnIndexesWithVarMap in
    match ast with
    | Local(v,e,scope) ->
        let newMap = StringMap.map (fun x -> x+1) map in
        let newMap = StringMap.add v 0 newMap in
        Local(v,f map e,f newMap scope)
    | RecFun(fname,arg,body,scope) ->
        let newMap = StringMap.map (fun x -> x+1) map in
        
        (* The function can be recursive, thus the function itself has a De Bruijn index in its body *)
        let newMapForBody = StringMap.map (fun x -> x+1) map in
        let newMapForBody = StringMap.add fname 0 newMapForBody in
        let newMapForBody = StringMap.add arg 1 newMapForBody in
        
        (* In the scope, the argument is no more defined, only the function has an index *)
        
        let newMapForScope = StringMap.add fname 0 newMap in
        
        RecFun(fname,arg,f newMapForBody body,f newMapForScope scope)
        
    | Fun(arg,body) ->
        let newMap = StringMap.map (fun x -> x+1) map in
        let newMap = StringMap.add arg 0 newMap in
        Fun(arg,f newMap body)
    | Eval(e1,e2) -> Eval(f map e1,f map e2)
    | Binary(op,e1,e2) -> Binary(op,f map e1,f map e2)
    | If(cond,e1,e2) -> If(f map cond,f map e1,f map e2)
    | Const (Var(v,i)) when not(StringMap.mem v map) -> raise (Failure ("Variable "^v^" is not defined"))
    | Const (Var(v,i)) -> Const (Var(v,StringMap.find v map))
    | _ as ast -> ast;;
    
let updateDeBruijnIndexes ast = updateDeBruijnIndexesWithVarMap (StringMap.empty) ast;;

let constToDBE = function
    | Int i -> DeBruijnExpression.Int i
    | Bool b -> DeBruijnExpression.Bool b
    | Var(s,i) -> DeBruijnExpression.Var i;;

let raw2dbe ast =
    let astWithDBI = updateDeBruijnIndexes ast in
    let rec toDBE = function
        | Local(s,e,scope) -> DeBruijnExpression.Local(toDBE e,toDBE scope)
        | RecFun(f,arg,body,scope) -> DeBruijnExpression.Local(toDBE body,toDBE scope)
        | Fun(arg,body) -> DeBruijnExpression.Fun(toDBE body)
        | Eval(e1,e2) -> DeBruijnExpression.Eval(toDBE e1,toDBE e2)
        | Binary(op,e1,e2) -> DeBruijnExpression.Binary(op,toDBE e1,toDBE e2)
        | If(cond,e1,e2) -> DeBruijnExpression.If(toDBE cond,toDBE e1,toDBE e2)
        | Const c -> DeBruijnExpression.Const(constToDBE c) in
    toDBE astWithDBI;;

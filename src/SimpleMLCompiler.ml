open UserInterface;;
open ML_syntax;;

open Printf;;
open Pervasives;;

open Interpreter;;

module StringMap = Map.Make(String);;

let file_path = Sys.argv.(1) in
let file = open_in file_path in

let print_file f : unit =
    let rec aux_print_file f first : unit =
        try
            if(not first)
            then
                print_newline();
            print_string (input_line f);
            aux_print_file f false
        with eof ->
            () in
            
    seek_in f 0;
    aux_print_file f true in
    
(* Pass 1 *)
(* We transform boolean operators into conditionnal expressions *)
    
let rec binaryBoolOpToCond = function
    | Local(v,e,scope) -> Local(v,binaryBoolOpToCond e,binaryBoolOpToCond scope)
    | RecFun(f,arg,body,scope) ->
        RecFun(f,arg,binaryBoolOpToCond body,binaryBoolOpToCond scope)
    | Fun(arg,body) -> Fun(arg,binaryBoolOpToCond body)
    | Eval(e1,e2) -> Eval(binaryBoolOpToCond e1,binaryBoolOpToCond e2)
    | Binary(op,e1,e2) when op==And ->
        If(binaryBoolOpToCond e1,binaryBoolOpToCond e2,Const (Bool false))
    | Binary(op,e1,e2) when op==Or ->
        If(binaryBoolOpToCond e1,Const (Bool true),binaryBoolOpToCond e2)
    | Binary(op,e1,e2) -> Binary(op,binaryBoolOpToCond e1,binaryBoolOpToCond e2)
    | If(cond,e1,e2) -> If(binaryBoolOpToCond cond,binaryBoolOpToCond e1,binaryBoolOpToCond e2)
    | _ as ast -> ast
    in

let pass1 = binaryBoolOpToCond in
    
(* Pass 2 *)
(* We compute De Bruijn indexes for leaf variables *)

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
    | _ as ast -> ast
    in
    
let updateDeBruijnIndexes ast = updateDeBruijnIndexesWithVarMap (StringMap.empty) ast in

let pass2 = updateDeBruijnIndexes in
    
let compile file =
    let lexbuf = Lexing.from_channel file in
    let ast = ref (Parser.program Lexer.token lexbuf) in
    ast := pass1 !ast;
    ast := pass2 !ast;
    !ast in
    
let p = compile file in
    printf "Input program :\n";
    print_file file;
    printf "\n\n";
    outputProgram p;;
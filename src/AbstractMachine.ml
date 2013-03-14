open Types
open List
open Utilities

exception NoMoreInstructions

type value = Int of int | Bool of bool | Closure of code*environment
and environment = value list
and stack = value list
and instruction =
    | Access of int
    | Apply
    | Cur of (instruction list)
    | Return
    | Let
    | Endlet
    | Branchneg of int
    | Branch of int
    | Op of op
    | Push of value
and code = instruction list

type state = code*environment*stack*stack

let apply_op op v1 v2 = match op,v1,v2 with
    | op,Int i,Int j -> Int (interpretIntBinOp op i j)
    | op,Bool i,Bool j -> Bool (interpretBoolBinOp op i j)
    | _ -> raise(failwith "Invalid operation")

let rec remove_nfirst l = function
    | 0 -> l
    | n -> remove_nfirst (tl l) (n-1)

let step (s:state) = match s with
    | (Access n)::c,e,s,r -> c,e,(nth e n)::s,r
    | Apply::c,e,(Closure(c0,e0))::v::s,r -> c0,v::e0,s,(Closure(c,e))::r
    | (Cur c1)::c,e,s,r -> c,e,[Closure(c1,e)],r
    | Return::c,e,v::s,(Closure(c0,e0))::r -> c0,e0,v::s,r
    | Let::c,e,v::s,r -> c,v::e,s,r
    | Endlet::c,v::e,s,r -> c,e,s,r
    | (Branch n)::c,e,s,r -> (remove_nfirst c n),e,s,r
    | (Branchneg n)::c,e,(Bool true)::s,r -> c,e,s,r
    | (Branchneg n)::c,e,(Bool false)::s,r -> (remove_nfirst c n),e,s,r
    | (Op op)::c,e,v::w::s,r -> c,e,(apply_op op v w)::s,r
    | (Push v)::c,e,s,r -> c,e,(v::s),r
    | [],e,s,r -> raise NoMoreInstructions
    | _ -> raise(failwith "Invalid instruction")

let createInitialState code = code,[],[],[]

let rec run (s:state) = match s with
    | [],e,s,r -> hd s
    | _ -> run (step s)
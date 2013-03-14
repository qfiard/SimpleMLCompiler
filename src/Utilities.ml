open Types

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
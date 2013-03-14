open Types

let interpretIntBinOp op i j = match op with
    | Plus -> i+j
    | Minus -> i-j
    | Times -> i*j
    | Div when j!=0 -> i/j
    | Div -> raise(failwith "Division by 0")
    | _ -> raise(failwith "Invalid operation on integers")

let interpretIntBinCompareOp op i j = match op with
    | Eq -> i==j
    | Neq -> i!=j
    | Le -> i<=j
    | Ge -> i>=j
    | Lt -> i<j
    | Gt -> i>j
    | _ -> raise(failwith "Invalid comparison")

let interpretBoolBinOp op i j = match op with
    | And -> i && j
    | Or -> i || j
    | Eq -> i == j
    | Neq -> i != j
    | _ -> raise(failwith "Invalid operation on booleans")
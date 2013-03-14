type op = Plus | Minus | Times | Div | And | Or

type dbindex = int

type exprType = IntType | BoolType | GenericType | FunType of (exprType->exprType)
open ML_syntax

type value = ConstVal of const | FunVal of expression
type state = value list
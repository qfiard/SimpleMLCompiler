open DeBruijnExpression

type value = ConstVal of const | FunVal of expression
type state = value list

val interpret : expression -> value
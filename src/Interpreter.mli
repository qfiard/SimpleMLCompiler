type value = ConstVal of DeBruijnExpression.const | FunVal of Expression.expression
type state = value list

val interpret_DBE : DeBruijnExpression.expression -> value

val interpret : Expression.expression -> value
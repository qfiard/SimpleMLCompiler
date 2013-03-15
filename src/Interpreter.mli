open Types

type value = ConstVal of DeBruijnExpression.const | FunVal of Expression.expression | RecFunVal of Expression.expression | SysCallVal of (value -> unit) | UnitVal
type state = value list

val interpret_DBE : DeBruijnExpression.expression -> value

val interpret : Expression.expression -> value

val outputValue : value -> unit
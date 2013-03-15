open Types

type value = ConstVal of DeBruijnExpression.const | FunVal of Expression.expression | RecFunVal of Expression.expression | SysCall of string | UnitVal
type state = value list

val interpret_DBE : DeBruijnExpression.expression -> value

val interpret : Expression.expression -> value

val outputValue : value -> unit
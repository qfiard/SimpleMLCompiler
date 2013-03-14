type expression =
    | Raw of ML_syntax.expression
    | DBE of DeBruijnExpression.expression
    | Code of AbstractMachine.code ;;
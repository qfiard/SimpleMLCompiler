module type Lexer = sig
    class lexer : unit ->
        object
        
        end
end;;

module MLLexer : Lexer;;
module type Parser = sig
    class parser : unit ->
        object
        
        end
end;;

module MLParser : Parser;;
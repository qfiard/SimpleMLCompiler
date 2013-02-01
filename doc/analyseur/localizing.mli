(* A locus is a location in the source text. *)
type locus

(* An extent is a pair of loci, giving the beginning and the end of *)
(* a syntactic element (instruction, expression, block, etc). *)
type extent

(* Set this reference to control the name of the file *)
val current_file_name: string ref

(* Lexer support. *)
val next_line: Lexing.lexbuf -> unit
val extent: Lexing.lexbuf -> extent

(* Parser support. *)
val extopt: extent option -> extent -> extent
val fromto: 'a * extent -> 'b * extent -> extent
val fromtoopt: 'a * extent -> 'b*extent -> 'c*(extent option) -> extent
val extent_unknown: unit -> extent

(* Display of extents. *)
val locus_to_string: locus -> string
val extent_to_string: extent -> string

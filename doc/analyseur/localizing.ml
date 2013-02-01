(* A locus is a location in the source text. *)
type locus =
    { locus_file_name:   string;
      locus_file_line:   int;
      locus_file_column: int;
      locus_file_char:   int; }

(* An extent is a pair of loci, giving the beginning and the end of *)
(* a syntactic element (instruction, expression, etc). *)
type extent =
    { extent_beg:       locus;
      extent_end:       locus;
      extent_unique_id: int }

(* Management of unique IDs *)
let cur_unique_id = ref 0
let get_unique_id () =
  let x = !cur_unique_id in
  incr cur_unique_id;
  x

(* Creation of new extents *)
let make_extent a b =
  { extent_beg = a;
    extent_end = b;
    extent_unique_id = get_unique_id () }
let extent_beg a = a.extent_beg
let extent_end a = a.extent_end
let extent_unique_id a = a.extent_unique_id

(* Lexer support *)
(* How to find locations in source *)
let current_line_num       = ref 1
let current_line_start_pos = ref 0
let current_file_name      = ref "*UNKNOWN*"
let next_line lexbuf =
  incr current_line_num;
  current_line_start_pos := Lexing.lexeme_end lexbuf;;
let find_locus_in_current_line n =
  { locus_file_name   = !current_file_name;
    locus_file_line   = !current_line_num;
    locus_file_column = n - !current_line_start_pos;
    locus_file_char   = n; }
let extent lexbuf =
  make_extent
    (find_locus_in_current_line (Lexing.lexeme_start lexbuf))
    (find_locus_in_current_line (Lexing.lexeme_end lexbuf))
    
(* Parser support *)
let extopt a b =
  match a with
  | None -> b
  | Some aa -> aa
let fromto (_,a) (_,b) =
  make_extent (extent_beg a) (extent_end b)
let fromtoopt a b (c,ceo) =
  match ceo with
  | None -> fromto a b
  | Some ce -> fromto a (c,ce)
let locus_unknown =
  { locus_file_name   = "<unknown>";
    locus_file_line   = -1;
    locus_file_column = -1;
    locus_file_char   = -1; }
let extent_unknown =
  let foo = make_extent locus_unknown locus_unknown in
  function () -> foo

(* Pretty-printing *)
let locus_to_string (l1: locus): string =
  Printf.sprintf "%s:%i:%i"
    l1.locus_file_name l1.locus_file_line l1.locus_file_column
let locus_pair_to_string (l1: locus) (l2: locus): string =
  if l1.locus_file_name<>l2.locus_file_name
  then Printf.sprintf "%s:%d.%d-%s:%d.%d:"
      l1.locus_file_name l1.locus_file_line l1.locus_file_column
      l2.locus_file_name l2.locus_file_line l2.locus_file_column
  else if l1.locus_file_line<>l2.locus_file_line
  then Printf.sprintf "%s:%d.%d-%d.%d:"
      l1.locus_file_name l1.locus_file_line l1.locus_file_column
      l2.locus_file_line l2.locus_file_column
  else if l1.locus_file_column<>l2.locus_file_column
  then Printf.sprintf "%s:%d.%d-%d:"
      l1.locus_file_name l1.locus_file_line l1.locus_file_column
      l2.locus_file_column
  else Printf.sprintf "%s:%d.%d:"
      l1.locus_file_name l1.locus_file_line l1.locus_file_column
let extent_to_string (ext: extent): string =
  locus_pair_to_string (extent_beg ext) (extent_end ext)

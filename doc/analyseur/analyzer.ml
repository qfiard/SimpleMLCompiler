open Simple_java_syntax

let main () =
  (* Parsing arguments *)
  let f_name = ref "" in
  Arg.parse [ ] (fun s -> f_name := s) "Mini-Java analyzer";
  (* Parsing of the source file *)
  let simple_java_prog =
    if String.compare !f_name "" = 0 then failwith "no program file given";
    Localizing.current_file_name := !f_name;
    let f_desc = open_in !f_name in
    let lexbuf = Lexing.from_channel f_desc in
    let java_prog =
      try Java_parser.program Java_lexer.token lexbuf
      with
      | e ->
          Printf.printf "Exception during parsing: %s\n"
	    (Printexc.to_string e);
          failwith "Stopped" in
    Simple_java_translate.tr_java_prog java_prog in
  Printf.printf "finished...\n"

let _ = main ()

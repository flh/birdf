open Bibtex

let _ =
  let lexbuf = Lexing.from_channel stdin in
  (*let count = ref 0 in*)
  try
    Bibtex_lex.outside lexbuf;
    while true do
      let result = Bibtex_parse.main Bibtex_lex.bibtex lexbuf in
      let g = to_birdf result in
      print_string (Rdf_xml.to_string g);
      (*let fh = open_out ("testbib_" ^(string_of_int !count)^".dot")  in
      output_string fh (Rdf_dot.dot_of_graph g);
      close_out fh;
      incr count; *)
      print_newline (); print_newline ()
    done
  with Bibtex_lex.Eof -> ()

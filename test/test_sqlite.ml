open Rdf_graph

let base = Rdf_iri.iri "http://example.com";;

let g = Rdf_graph.open_graph ~options:["storage", "sqlite"; "filename",
"test.sqlite"] base;;

let a = Rdf_term.Blank_ (g.Rdf_graph.new_blank_id());;
let b = Rdf_term.Blank_ (g.Rdf_graph.new_blank_id());;

g.add_triple a Rdf_foaf.foaf_knows b;
g.add_triple a Rdf_foaf.foaf_name (Rdf_term.(Literal {
  lit_type=None; lit_value = "Florian Hatat"; lit_language=None}));
g.add_triple a Rdf_foaf.foaf_homepage (Rdf_term.Iri (Rdf_iri.iri "http://florian.hatat.fr/"));
g.add_triple a Rdf_rdf.rdf_type (Rdf_term.Iri Rdf_foaf.foaf_Person);;

let dataset = Rdf_ds.simple_dataset g;;

open Rdf_sparql

let query = try query_from_string "
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
SELECT ?name ?mbox ?hpage WHERE {
?x foaf:name ?name .
?x rdf:type foaf:Person .
OPTIONAL { ?x foaf:mbox ?mbox } .
OPTIONAL { ?x foaf:homepage ?hpage }
}"
with Error e -> failwith (string_of_error e);;


let solutions = select ~base dataset query

let print_sol =
  let print varname term =
     Printf.printf "%s => %s\n" varname (Rdf_term.string_of_term term)
  in
  fun sol ->
    print_endline "Solution:";
    solution_iter print sol ;
    print_newline()

let _ = List.iter print_sol solutions

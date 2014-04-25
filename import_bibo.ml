let ns = function
  | "terms"   -> "http://purl.org/dc/terms/"
  | "foaf"    -> "http://xmlns.com/foaf/0.1/"
  | "vann"    -> "http://purl.org/vocab/vann/"
  | "owl"     -> "http://www.w3.org/2002/07/owl#"
  | "bibo"    -> "http://purl.org/ontology/bibo/"
  | "skos"    -> "http://www.w3.org/2004/02/skos/core#"
  | "xsd"     -> "http://www.w3.org/2001/XMLSchema#"
  | "event"   -> "http://purl.org/NET/c4dm/event.owl#"
  | "rdfs"    -> "http://www.w3.org/2000/01/rdf-schema#"
  | "status"  -> "http://purl.org/ontology/bibo/status/"
  | "degrees" -> "http://purl.org/ontology/bibo/degrees/"
  | "rdf"     -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  | "schema"  -> "http://schemas.talis.com/2005/address/schema#"
  | "ns"      -> "http://www.w3.org/2003/06/sw-vocab-status/ns#"
  | "prism"   -> "http://prismstandard.org/namespaces/1.2/basic/"
  | _ -> raise Not_found

let output_prefix = "bibo"

let base = Rdf_iri.iri (ns "bibo")
let g = Rdf_graph.open_graph base

let resolve_entities e =
  try Some (ns e)
  with _ -> None

let _ =
  let fh = open_in Sys.argv.(1) in
  let input = Xmlm.make_input ~strip:true ~entity:resolve_entities (`Channel fh) in
  let res = Rdf_xml.from_input g input in
  close_in fh;
  res

let owl = Rdf_iri.iri (ns "owl")
let owl_ = Rdf_iri.append owl
let owl_ObjectProperty = owl_"ObjectProperty"
let owl_DatatypeProperty = owl_"DatatypeProperty"
let owl_Class = owl_"Class"

open Camlp4.PreCast

let sanitize s =
  let res = String.copy s in
  for i = 0 to String.length res - 1 do
    if res.[i] = '/' then res.[i] <- '_'
  done;
  res

let generate_bibo =
  let bibo_len = String.length (ns "bibo") in
  function
  | Rdf_term.Iri i ->
      let term = Rdf_iri.string i in
      if String.sub term 0 bibo_len = (ns "bibo")
      then
        let b = sanitize (String.sub term bibo_len (String.length term - bibo_len)) in
        let _loc = Loc.ghost in
        <:str_item<let $lid:output_prefix ^ "_" ^ b$ = $lid: output_prefix^"_"$ $str:b$>>

      else raise Not_found
  | _ -> raise Not_found

let rec filter_map f = function
  | [] -> []
  | a :: l ->
      (try
        let x = f a in fun () -> x :: (filter_map f l)
      with _ -> fun () -> filter_map f l) ()

let bibos =
  g.Rdf_graph.subjects_of
    ~pred:(Rdf_iri.iri ((ns "rdfs") ^ "isDefinedBy"))
    ~obj:(Rdf_term.term_of_literal_string ~typ:(Rdf_iri .iri ((ns "xsd") ^ "anyURI")) (ns "bibo"))

let classes =
let sparql_classes = Rdf_sparql.query_from_string
  "
  PREFIX owl: <http://www.w3.org/2002/07/owl#>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
  SELECT ?member
  WHERE {
    ?member rdf:type ?class .
    ?class rdf:type owl:Class .
    ?class rdfs:isDefinedBy \"http://purl.org/ontology/bibo/\"^^xsd:anyURI
  }
  "
and ds = Rdf_ds.simple_dataset g in
let solutions = Rdf_sparql.select ~base ds sparql_classes in
List.map (fun sol -> Rdf_sparql.get_term sol "member") solutions


let _loc = Loc.ghost in
Printers.OCaml.print_implem (
  Ast.stSem_of_list (
  <:str_item<
let bibo = Rdf_iri.iri $str:(ns "bibo")$
let bibo_ = Rdf_iri.append bibo
  >>
  :: (filter_map generate_bibo (bibos @ classes))))

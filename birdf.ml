open Rdf_term

let usual_namespaces =
  [
    (Rdf_foaf.foaf, "foaf");
    (Rdf_rdf.dc, "dc");
    (Rdf_bibo.bibo, "bibo");
    (Rdf_dc.terms, "dcterm");
  ]

let open_graph url =
  let g = Rdf_graph.open_graph url in
  List.iter (fun (ns, name) -> g.Rdf_graph.add_namespace ns name) usual_namespaces;
  g

let mk_literal_string s =
  mk_literal ~typ:Rdf_rdf.xsd_string s
let term_of_string s =
  Literal (mk_literal_string s)

let create_node g url =
  match url with
  | None -> Blank_ (g.Rdf_graph.new_blank_id())
  | Some url -> term_of_iri_string url

let create_author g ?url name =
  let author = create_node g url in
  g.Rdf_graph.add_triple ~sub:author ~pred:Rdf_rdf.rdf_type ~obj:(Iri Rdf_foaf.foaf_Person);
  g.Rdf_graph.add_triple ~sub:author ~pred:Rdf_foaf.foaf_name ~obj:(term_of_literal_string name);
  author

let find_author g name =
  try List.hd
    (g.Rdf_graph.subjects_of ~pred:Rdf_foaf.foaf_name ~obj:(term_of_literal_string name))
  with _ -> raise Not_found

let add_publication_prop g pub (property : [> ]) =
  try
    let (pred, obj) = match property with
    | `Article -> (Rdf_rdf.rdf_type, Iri Rdf_bibo.bibo_Article)
    | `Thesis ->  (Rdf_rdf.rdf_type, Iri Rdf_bibo.bibo_Thesis)
    | `Creator a | `Author a -> (Rdf_dc.dc_creator, find_author g a)
    | `Title t -> (Rdf_dc.dc_title, (term_of_literal_string t))
    | `Date d -> (Rdf_dc.dc_date, term_of_datetime ~d:d ())
    | `Issuer i -> (Rdf_bibo.bibo_issuer, (term_of_literal_string i))
    | `Uri u -> (Rdf_bibo.bibo_uri, (term_of_literal_string u))
    | `Doi d -> (Rdf_bibo.bibo_doi, (term_of_literal_string d))
    | `Publisher p -> (Rdf_dc.terms_publisher, (term_of_literal_string p))
    | `Isbn i -> (Rdf_bibo.bibo_isbn, (term_of_literal_string i))
    | `Volume v -> (Rdf_bibo.bibo_volume, (term_of_literal_string v))
    | `Issue i -> (Rdf_bibo.bibo_issue, (term_of_literal_string i))
    | `Number n -> (Rdf_bibo.bibo_number, (term_of_literal_string n))
    | `Pages p -> (Rdf_bibo.bibo_pages, (term_of_literal_string p))
    | _ -> raise Not_found
    in g.Rdf_graph.add_triple ~sub:pub ~pred:pred ~obj:obj
  with Not_found -> ()

let add_publication g ?url props =
  let pub = create_node g url in
  List.iter (add_publication_prop g pub) props

let extract_publication g pub =
  let subg = open_graph (Rdf_iri.iri "http://example.com/") in
  List.iter subg.Rdf_graph.add_triple_t (g.Rdf_graph.find ~sub:pub ());
  subg

let export_publication g pub =
  let subg = extract_publication g pub in
  let (fname, fh) = Filename.open_temp_file "birdf_" ".rdf" in
  output_string fh (Rdf_xml.to_string subg);
  close_out fh;
  fname

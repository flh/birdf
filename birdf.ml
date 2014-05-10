open Rdf_term
open Rdf_graph

let usual_namespaces =
  [
    (Rdf_foaf.foaf, "foaf");
    (Rdf_rdf.dc, "dc");
    (Bibo.bibo, "bibo");
    (Rdf_dc.terms, "dcterm");
  ]

let open_graph url =
  let g = open_graph url in
  List.iter (fun (ns, name) -> g.add_namespace ns name) usual_namespaces;
  g

let mk_literal_string s =
  mk_literal ~typ:Rdf_rdf.xsd_string s
let term_of_string s =
  Literal (mk_literal_string s)

let create_node g url =
  match url with
  | None -> Blank_ (g.new_blank_id())
  | Some url -> term_of_iri_string url

(** {2} Author management *)

(** Creating an empty author node, i.e., a node which is only a
  foaf:Person. More predicates can be added later. *)
let create_author ?url g =
  let author = create_node g url in
  g.add_triple ~sub:author ~pred:Rdf_rdf.rdf_type ~obj:(Iri Rdf_foaf.foaf_Person);
  author

(** Adding a foaf:name to an author. *)
let author_name g author name =
  g.add_triple ~sub:author ~pred:Rdf_foaf.foaf_name ~obj:(term_of_literal_string name)

(** Finding an author using its foaf:name *)
let find_author g name =
  try List.hd
    (g.subjects_of ~pred:Rdf_foaf.foaf_name ~obj:(term_of_literal_string name))
  with _ -> raise Not_found

(** Basic template for creating an author.
  This function creates an author, identified by an optional [?url], and
  adds some common empty properties to him. Useful when used on an empty
  graph to provide the user with a sensible template to edit. *)
let author_template ?url g =
  let author = create_author ?url g in
  g.add_triple ~sub:author ~pred:Rdf_foaf.foaf_familyName ~obj:(term_of_literal_string "");
  g.add_triple ~sub:author ~pred:Rdf_foaf.foaf_givenName ~obj:(term_of_literal_string "");
  g.add_triple ~sub:author ~pred:Rdf_foaf.foaf_homepage ~obj:(Rdf_term.Iri (Rdf_iri.iri "http://"));
  g.add_triple ~sub:author ~pred:Rdf_foaf.foaf_mbox ~obj:(Rdf_term.Iri (Rdf_iri.iri "mailto:"))

(** {2} Merging nodes

  When importing bibliographical data from different sources, some
  entries may be inserted multiple times, if we couldn't detect at that
  time that they should have been considered equivalent. They can be
  merged afterwards using the following functions.
 *)

(** Replace the suject in triple [(osub, pred, obj)] with a new one
  [nsub]. If the triple [(nsub, pred, obj)] already exists, then the
  triple to replace is simply removed from the graph.
  *)
let replace_subject g nsub (osub, opred, oobj) =
  if not (g.exists ~sub:nsub ~pred:opred ~obj:oobj ())
  then g.add_triple nsub opred oobj;
  g.rem_triple ~sub:osub ~pred:opred ~obj:oobj

(** Replace the object in triple [(sub, pred, oobj)] with a new one
  [nobj]. If the triple [(sub, pred, nobj)] already exists, then the
  triple to replace is simply removed from the graph.
  *)
let replace_object g nobj (osub, opred, oobj) =
  if not (g.exists ~sub:osub ~pred:opred ~obj:nobj ())
  then g.add_triple osub opred nobj;
  g.rem_triple ~sub:osub ~pred:opred ~obj:oobj

(** Calling [merge nodes g aut1 aut2] replaces all occurrences of the
  node [aut2] in the graph with [aut2]. This function uses
  {!replace_subject} and {!replace_object} to ensure that no duplicate
  triple is create while merging. *)
let merge_nodes g aut1 aut2 =
  List.iter (replace_subject g aut1) (g.find ~sub:aut2 ());
  List.iter (replace_object  g aut1) (g.find ~obj:aut2 ())

(** {2} Publication management *)

let add_publication_prop g pub (property : [> ]) =
  try
    let (pred, obj) = match property with
    | `Article -> (Rdf_rdf.rdf_type, Iri Bibo.bibo_Article)
    | `Thesis ->  (Rdf_rdf.rdf_type, Iri Bibo.bibo_Thesis)
    | `Creator a | `Author a -> (Rdf_dc.dc_creator, find_author g a)
    | `Title t -> (Rdf_dc.dc_title, (term_of_literal_string t))
    | `Date d -> (Rdf_dc.dc_date, term_of_datetime ~d:d ())
    | `Issuer i -> (Bibo.bibo_issuer, (term_of_literal_string i))
    | `Uri u -> (Bibo.bibo_uri, (term_of_literal_string u))
    | `Doi d -> (Bibo.bibo_doi, (term_of_literal_string d))
    | `Publisher p -> (Rdf_dc.terms_publisher, (term_of_literal_string p))
    | `Isbn i -> (Bibo.bibo_isbn, (term_of_literal_string i))
    | `Volume v -> (Bibo.bibo_volume, (term_of_literal_string v))
    | `Issue i -> (Bibo.bibo_issue, (term_of_literal_string i))
    | `Number n -> (Bibo.bibo_number, (term_of_literal_string n))
    | `Pages p -> (Bibo.bibo_pages, (term_of_literal_string p))
    | _ -> raise Not_found
    in g.add_triple ~sub:pub ~pred:pred ~obj:obj
  with Not_found -> ()

let add_publication g ?url props =
  let pub = create_node g url in
  List.iter (add_publication_prop g pub) props

let extract_publication g pub =
  let subg = open_graph (Rdf_iri.iri "http://example.com/") in
  List.iter subg.add_triple_t (g.find ~sub:pub ());
  subg

let export_publication g pub =
  let subg = extract_publication g pub in
  let (fname, fh) = Filename.open_temp_file "birdf_" ".rdf" in
  output_string fh (Rdf_xml.to_string subg);
  close_out fh;
  fname

type bibtex =
  {
    pubtype : string;
    key : string;
    fields : (string * string) list;
  }

let parse_authors a =
  ["moi"]

let parse_date d =
  []

let known_pubtypes =
  [
    ("article", `Article);
    ("techreport", `Techreport);
    ("unpublished", `Unpublished);
    ("misc", `Misc);
    ("inproceedings", `Inproceedings);
    ("proceedings", `Proceedings);
    ("incollection", `Incollection);
    ("book", `Book);
    ("phdthesis", `PhdThesis);
  ]

let pubtype_to_birdf p =
  try List.assoc p known_pubtypes
  with Not_found -> `Unknown

let prop_to_birdf p v =
  match p with
  | "title" -> [`Title v]
  | "author" -> List.map (fun a -> `Author a) (parse_authors v)
  | "date" -> []
  | "url" -> [`Uri v]
  | "publisher" -> [`Publisher v]
  | "isbn" -> [`Isbn v]
  | "volume" -> [`Volume v]
  | "number" -> [`Number v]
  | "issue" -> [`Issue v]
  | "pages" -> [`Pages v]
  | _ -> []

let to_birdf e =
  let g = Birdf.open_graph (Rdf_iri.iri "http://blah.org") in
  let pub = Rdf_term.(Blank_ (blank_id_of_string e.key)) in
  Birdf.add_publication_prop g pub (pubtype_to_birdf e.pubtype);
  List.iter (Birdf.add_publication_prop g pub)
    (List.flatten (List.map (fun (p, v) -> prop_to_birdf p v) e.fields));
  g

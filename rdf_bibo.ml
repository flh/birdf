let bibo = Rdf_iri.iri "http://purl.org/ontology/bibo/"
let bibo_ = Rdf_iri.append bibo
let bibo_Article = bibo_"Article"
let bibo_Thesis  = bibo_"Thesis"
let bibo_issuer  = bibo_"issuer"
let bibo_uri     = bibo_"uri"
let bibo_doi     = bibo_"doi"
let bibo_isbn    = bibo_"isbn"
let bibo_volume  = bibo_"volume"
let bibo_issue   = bibo_"issue"
let bibo_number  = bibo_"number"
let bibo_pages   = bibo_"pages"

let bibo_degree  = bibo_"degree"
let bibo_degree  = bibo_"degrees/"
let bibo_degrees_ = Rdf_iri.append bibo_degree
let bibo_degrees_phd = bibo_degrees_"phd"

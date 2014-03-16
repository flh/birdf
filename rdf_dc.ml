let dc_date = Rdf_rdf.dc_ "date"
let dc_creator = Rdf_rdf.dc_ "creator"
let dc_title = Rdf_rdf.dc_ "title"

let terms = Rdf_iri.iri "http://purl.org/dc/terms/"
let terms_ = Rdf_iri.append terms
let terms_publisher = terms_"publisher"

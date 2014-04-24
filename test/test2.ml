#use "topfind";;
#require "rdf";;
#require "sqlite3";;
#load "sqlite_storage.cmo";;

let g = Sqlite_storage.open_graph ~options:["filename", "test.sqlite"] (Rdf_iri.iri "urn:hello")

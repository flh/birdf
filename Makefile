DEBUG:=-g

test/test: test/test.ml
	ocamlfind ocamlc $(DEBUG) -o $@ -package rdf -linkpkg $<

test/test_sqlite: sqlite_storage.cmo test/test_sqlite.cmo
	ocamlfind ocamlc $(DEBUG) -linkpkg -package sqlite3,rdf -o $@ $^

import_bibo.ml.depends: import_bibo.ml
	ocamlfind ocamldep -pp camlp4of -package rdf $^ > $@
import_bibo.cmo: import_bibo.ml
	ocamlfind ocamlc -c $(DEBUG) -pp camlp4of -package rdf -o $@ $^
import_bibo: import_bibo.cmo
	ocamlfind ocamlc $(DEBUG) -linkpkg -package camlp4.fulllib,rdf -o $@ $^
bibo.mli: bibo.owl import_bibo
	ocamlfind ocamlc -i -pp ./import_bibo -package rdf -impl $< > $@
bibo.cmo: bibo.owl import_bibo bibo.cmi
	ocamlfind ocamlc -c -pp ./import_bibo -package rdf -impl $< -o $@

sqlite_storage.cmo: sqlite_storage.ml
	ocamlfind ocamlc $(DEBUG) -c -package sqlite3,rdf -c $< $@

SOURCES := $(wildcard *.ml) $(wildcard main/*.ml) $(wildcard bibtex/*.ml) bibtex/bibtex_parse.mli bibtex/bibtex_parse.ml bibtex/bibtex_lex.ml
INCLUDES := -I bibtex -I main
PACKAGES := -package rdf

# Main executables
main/birdf-init: main/birdf-init.cmo sqlite_storage.cmo
	ocamlfind ocamlc $(INCLUDES) -o $@ -linkpkg -package rdf,sqlite3 sqlite_storage.cmo path.cmo util.cmo $<

# Bibtex parsing
bibtex/test_bibtex.ml.depends: bibtex/bibtex_lex.ml bibtex/bibtex_parse.ml
bibtex/bibtex_parse.mli: bibtex/bibtex_parse.ml ;
bibtex/bibtex_parse.ml: bibtex/bibtex_parse.mly
	ocamlyacc $<
bibtex/bibtex_lex.ml: bibtex/bibtex_lex.mll bibtex/bibtex_parse.ml
	ocamllex $<

bibtex/test_bibtex: bibo.cmo birdf.cmo bibtex/bibtex.cmo bibtex/bibtex_parse.cmo bibtex/bibtex_lex.cmo bibtex/test_bibtex.cmo
	ocamlfind ocamlc $(DEBUG) -o $@ $(PACKAGES) -linkpkg $^

birdf.cmo: bibo.cmo

%.depends: %
	ocamlfind ocamldep $(PACKAGES) $(INCLUDES) $< > $@

DEPENDS := $(addsuffix .depends, $(SOURCES))
-include $(DEPENDS)

clean:
	rm -f $(SOURCES:.ml=.cmo) $(SOURCES:.ml=.cmi) \
	  bibtex/bibtex_parse.mli bibtex/bibtex_parse.ml \
	  bibo.ml bibo.mli bibo.cmo bibo.cmi import_bibo \
	  bibtex/test_bibtex \
	  bibtex/bibtex_lex.ml $(DEPENDS)

%.cmo: %.ml
	ocamlfind ocamlc $(DEBUG) -c $(PACKAGES) $(INCLUDES) $<
%.cmi: %.mli
	ocamlfind ocamlc -c $(PACKAGES) $(INCLUDES) $<

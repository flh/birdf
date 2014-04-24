DEBUG:=-g

test/test: test/test.ml
	ocamlfind ocamlc $(DEBUG) -o test -package rdf -linkpkg $<

test/test_sqlite: sqlite_storage.cmo test/test_sqlite.cmo
	ocamlfind ocamlc $(DEBUG) -linkpkg -package sqlite3,rdf -o $@ $^

sqlite_storage.cmo: sqlite_storage.ml
	ocamlfind ocamlc $(DEBUG) -c -package sqlite3,rdf -c $< $@

SOURCES := $(wildcard *.ml) bibtex/bibtex.ml bibtex/bibtex_parse.mli bibtex/bibtex_parse.ml bibtex/bibtex_lex.ml bibtex/test_bibtex.ml
INCLUDES := -I bibtex
PACKAGES := -package rdf

bibtex/test_bibtex.ml.depends: bibtex/bibtex_lex.ml bibtex/bibtex_parse.ml
bibtex/bibtex_parse.mli: bibtex/bibtex_parse.ml ;
bibtex/bibtex_parse.ml: bibtex/bibtex_parse.mly
	ocamlyacc $<
bibtex/bibtex_lex.ml: bibtex/bibtex_lex.mll bibtex/bibtex_parse.ml
	ocamllex $<

bibtex/test_bibtex: rdf_bibo.cmo rdf_dc.cmo birdf.cmo bibtex/bibtex.cmo bibtex/bibtex_parse.cmo bibtex/bibtex_lex.cmo bibtex/test_bibtex.cmo
	ocamlfind ocamlc $(DEBUG) -o $@ $(PACKAGES) -linkpkg $^

%.depends: %
	ocamlfind ocamldep $(PACKAGES) $(INCLUDES) $< > $@

DEPENDS := $(addsuffix .depends, $(SOURCES))
-include $(DEPENDS)

clean:
	rm -f $(SOURCES:.ml=.cmo) $(SOURCES:.ml=.cmi) \
	  bibtex/bibtex_parse.mli bibtex/bibtex_parse.ml \
	  bibtex/bibtex_lex.ml $(DEPENDS)

%.cmo: %.ml
	ocamlfind ocamlc $(DEBUG) -c $(PACKAGES) $(INCLUDES) $<
%.cmi: %.mli
	ocamlfind ocamlc -c $(PACKAGES) $(INCLUDES) $<

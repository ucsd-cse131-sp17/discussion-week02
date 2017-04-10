WEEK = week02

$(WEEK): helper.ml $(WEEK).ml test.ml
	ocamlfind ocamlc -o $@ -package oUnit -linkpkg -g $^

.PHONY: clean

clean:
	rm -f *.cmi *.cmo $(WEEK) oUnit-suite*

WEEK = week02

all: $(WEEK)

$(WEEK): helper.ml $(WEEK).ml test.ml
	ocamlfind ocamlc -o $@ -package oUnit -linkpkg -g $^

.PHONY: clean all

clean:
	rm -f *.cmi *.cmo $(WEEK) oUnit-suite*

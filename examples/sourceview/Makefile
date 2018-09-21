all: example2

example2: example2.ml
	ocamlfind ocamlopt -unsafe-string -package lablgtk2.sourceview2 -linkpkg -o $@ $<

clean:
	rm *.cm* *.o example2

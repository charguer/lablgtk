all: example2

example2: example2.ml
	ocamlfind ocamlopt -g -unsafe-string -package lablgtk2.sourceview2 -linkpkg -o $@ $<

run: all
	./example2

clean:
	rm *.cm* *.o example2

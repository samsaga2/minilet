all:
	ocamlbuild -use-menhir -menhir "menhir -v" -use-ocamlfind minilet.native

clean:
	ocamlbuild -clean

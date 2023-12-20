INTERFACES = tpParse.mli
SOURCES    = ast.ml tpParse.ml tpLex.ml verif.ml code.ml main.ml
GENERATED  = tpLex.ml tpLex.mli ast.mli tpParse.ml tpParse.mli tpParse.automaton tpParse.conflicts code.mli verif.mli main.mli

tp: ast.mli tpParse.mli $(SOURCES)
	ocamlc -g -o tp $(INTERFACES)
	ocamlc -g -o tp $(SOURCES)


tpLex.ml: tpLex.mll tpParse.mli ast.mli
	ocamllex tpLex.mll

# Les dépendances ci-dessous seront probablement à mettre à jour en fonction
# de l'organisation de votre code et des relations entre fichiers/modules
ast.mli: ast.ml
	ocamlc -c -g ast.ml

main.mli: ast.mli code.mli main.ml
	ocamlc -c -g main.ml

code.mli: ast.mli code.ml
	ocamlc -c code.ml

verif.mli: ast.mli verif.ml
	ocamlc -c verif.ml

tpParse.mli : ast.mli tpParse.mly 
	menhir --dump --explain --infer  tpParse.mly

clean:
	rm -rf  tp out.txt  *.o *.cmi *.cmo *.cmx *~ $(GENERATED)

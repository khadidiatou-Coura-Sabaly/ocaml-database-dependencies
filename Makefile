
# Makefile du projet OCaml
# Pour compiler : make
# Pour nettoyer les fichiers générés : make clean
# Pour nettoyer les fichiers html et css : make htmlclean

BIN = test

MODULES = src/type.cmo src/utils.cmo src/table.cmo src/deps.cmo src/normalisation.cmo
TEST_MODULES = tests/tests_operations.cmo tests/tests_deps.cmo
OBJECTS = $(MODULES) $(TEST_MODULES) src/projet.cmo

.PHONY: clean htmlclean

$(BIN): $(OBJECTS)
	ocamlc -g -I src -I tests $(OBJECTS) -o $(BIN)

src/type.cmo: src/type.ml src/type.cmi
src/utils.cmo: src/utils.ml src/utils.cmi src/type.cmi
src/table.cmo: src/table.ml src/table.cmi src/type.cmi src/utils.cmi
src/deps.cmo: src/deps.ml src/deps.cmi src/type.cmi src/utils.cmi src/table.cmi
src/normalisation.cmo: src/normalisation.ml src/normalisation.cmi src/deps.cmi

tests/tests_operations.cmo: tests/tests_operations.ml src/type.cmi src/table.cmi
tests/tests_deps.cmo: tests/tests_deps.ml src/deps.cmi src/normalisation.cmi

src/projet.cmo: src/projet.ml tests/tests_operations.cmo tests/tests_deps.cmo

%.cmo: %.ml
	ocamlc -g -I src -I tests -c $<

%.cmi: %.mli
	ocamlc -g -I src -I tests -c $<

htmlclean:
	rm -f *.html *.css

clean:
	rm -f src/*.cmo src/*.cmi tests/*.cmo tests/*.cmi $(BIN)

#
# Pure OCaml, no packages, no _tags 
#

# bin-annot is required for Merlin and other IDE-like tools

OCB_FLAGS = 
OCB = 		ocamlbuild  -use-ocamlfind $(OCB_FLAGS)
MAIN = main

all:    native 

clean:
	$(OCB) -clean
	rm src/*~

native: 
	$(OCB) $(MAIN).native

byte:
	$(OCB) $(MAIN).byte

profile:
	$(OCB) -tag profile $(MAIN).native

debug:
	$(OCB) -tag debug $(MAIN).byte

test: 	native
	./$(MAIN).native 

.PHONY: all clean byte native profile debug test

%.inferred.mli:
	@$(OCB) src/$@ && cat _build/src/$@

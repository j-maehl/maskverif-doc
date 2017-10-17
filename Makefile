# --------------------------------------------------------------------
OCB_FLAGS :=
OCB       := ocamlbuild -use-ocamlfind $(OCB_FLAGS)
MAIN      := main

# --------------------------------------------------------------------
.PHONY: all clean byte native profile debug test

all: native 

clean:
	$(OCB) -clean; rm -f src/*~

native: 
	$(OCB) $(MAIN).native

byte:
	$(OCB) $(MAIN).byte

profile:
	$(OCB) -tag profile $(MAIN).native

debug:
	$(OCB) -tag debug $(MAIN).byte

test:	native
	./$(MAIN).native 

%.inferred.mli:
	@$(OCB) src/$@ && cat _build/src/$@

# --------------------------------------------------------------------
OCB_FLAGS := 
MAIN      := main
MAINPARSE := main_input
ILANG     := ilang
UNAME_S   := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
OCB_FLAGS += -lflags -cclib,-lrt
endif

OCB := ocamlbuild -use-ocamlfind $(OCB_FLAGS)

# --------------------------------------------------------------------
.PHONY: all clean byte native profile debug test

all: native 

clean:
	$(OCB) -clean; rm -f src/*~

native: 
	$(OCB) -tag debug $(MAIN).native $(MAINPARSE).native 

byte:
	$(OCB) $(MAIN).byte $(MAINPARSE).byte

profile:
	$(OCB) -tag profile $(MAIN).native $(MAINPARSE).byte

debug:
	$(OCB) -tag debug $(MAIN).byte $(MAINPARSE).byte

test:	native
	./$(MAIN).native 

%.inferred.mli:
	@$(OCB) src/$@ && cat _build/src/$@

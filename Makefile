# --------------------------------------------------------------------
OCB_FLAGS := 
MAIN      := main
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
	$(OCB) -clean; rm -f *~ src/*~ maskverif

native: 
	$(OCB) -tag debug $(MAIN).native 
	ln -sf $(MAIN).native maskverif
byte:
	$(OCB) $(MAIN).byte $(MAIN).byte

profile:
	$(OCB) -tag profile $(MAIN).native 

debug:
	$(OCB) -tag debug $(MAIN).byte 

test:	native
	./$(MAIN).native 

%.inferred.mli:
	@$(OCB) src/$@ && cat _build/src/$@

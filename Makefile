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
.PHONY: all clean byte native profile debug install

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

install: maskverif.cma maskverif.cmxa maskverif.cmxs maskverif.cmx
	-ocamlfind remove maskverif
	ocamlfind install maskverif \
		META _build/src/maskverif.cma \
		_build/src/maskverif.cmxa _build/src/maskverif.cmxs \
		_build/src/maskverif.cmx _build/src/maskverif.cmi \
		_build/src/maskverif.a _build/src/maskverif.cmo \
		_build/src/maskverif.cmt _build/src/maskverif.annot

maskverif.%:
	$(OCB) $(@)

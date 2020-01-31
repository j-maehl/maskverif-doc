# --------------------------------------------------------------------
OCB_FLAGS :=
MAIN      := main
ILANG     := ilang
UNAME_S   := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
OCB_FLAGS += -lflags -cclib,-lrt
SHELL     := /bin/bash
endif

OCB := ocamlbuild -use-ocamlfind $(OCB_FLAGS)

# --------------------------------------------------------------------
.PHONY: all clean byte native profile debug test library install uninstall

all: native library install

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

library: shrcntlow maskverif.cmx maskverif.cmo maskverif.cma maskverif.cmxa maskverif.cmxs
	-ocamlfind remove maskverif 2> /dev/null
	ocamlfind install maskverif META \
		_build/src/maskverif.{cmi,cmx,cma,cmo,cmxs,cmxa,a,o} \
		src/*.{mli,ml,mll,mly,c,mllib,mlpack} \
		_build/src/shrcnt_low.o \
		_build/src/*.{cmt,cmti}
	rm `ocamlfind query maskverif`/main.* || true

install: uninstall native library
	@if [[ ":${PATH}:" == *":${HOME}/.local/bin:"* ]]; then\
	   mkdir -p "${HOME}/.local/bin/" && \
	   cp $(MAIN).native "${HOME}/.local/bin/maskverif" && \
	   echo "installed maskverif to '${HOME}/.local/bin/maskverif'"; \
	elif [[ ":${PATH}:" == *":${HOME}/bin:"* ]]; then\
	   mkdir -p "${HOME}/bin/" && \
	   cp $(MAIN).native "${HOME}/bin/maskverif" && \
	   echo "installed maskverif to '${HOME}/bin/maskverif'"; \
	else\
	  echo "Your path is missing ~/bin or ~/.local/bin, refusing to install executable.";\
	fi

uninstall:
	-ocamlfind remove maskverif 2> /dev/null
ifneq (,$(wildcard ${HOME}/.local/bin/maskverif))
	rm "${HOME}/.local/bin/maskverif"
endif
ifneq (,$(wildcard ${HOME}/bin/maskverif))
	rm "${HOME}/bin/maskverif"
endif

maskverif.%: shrcntlow
	$(OCB) shrcnt_low.o -cflags -cclib,-lshrcnt_low $(@)

shrcntlow:
	$(OCB) src/shrcnt_low.o

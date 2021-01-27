# --------------------------------------------------------------------
UNAME_S   := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
SHELL     := /bin/bash
endif

# --------------------------------------------------------------------
.PHONY: clean build install all

all: install

build:
	dune build

clean:
	dune clean; rm -f *~ src/*~ maskverif

install:
	dune build @install && ln -s main.exe maskverif

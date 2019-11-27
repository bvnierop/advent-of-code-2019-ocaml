# Our default target is all. It does nothing.
all:

# Disable implicit rules
.SUFFIXES:

# Phony targets are required because none of these require any files
.PHONY: deps init build test utop clean

deps:
	opam update
	opam install . --deps-only

init:
	[ -d _opam ] || [ -d .opam ] || opam switch create . ocaml-base-compiler.4.08.1

test:
	dune runtest

utop:
	dune utop

clean:
	dune clean

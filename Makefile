#
# The latte-checker tool, build by ocamlbuild
#
# Pure OCaml, package from Opam, two directories
#

# - The -I flag introduces sub-directories
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain

.PHONY: all clean byte native profile debug sanity test

OCB_FLAGS = -use-ocamlfind -I src -I lib
OCB = ocamlbuild $(OCB_FLAGS)

all: byte # native byte # profile debug

clean:
	$(OCB) -clean

native: sanity
	$(OCB) main.native

byte: sanity
	$(OCB) main.byte

profile: sanity
	$(OCB) -tag profile main.native

debug: sanity
	$(OCB) -tag debug main.byte

# check that packages can be found
sanity:
	ocamlfind query oUnit
	ocamlfind query batteries

syntax_tests: byte
	$(OCB) syntax_tests.byte

test: syntax_tests
	./syntax_tests.byte # .native ?


cleanall: clean
	find ./ -name "*~" -exec rm -f {} \;


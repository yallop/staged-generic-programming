OCAMLBUILD=ocamlbuild -use-ocamlfind -ocamlc '-toolchain metaocaml ocamlc'

all: metasyb_staged.cma

full: all test xpath

test: test-build
	cd _build/lib && ../../test_syb.byte

test-build: all
	$(OCAMLBUILD) test_syb.byte

xpath: all
	$(OCAMLBUILD) xpath/xpath.byte

syntax:
	$(OCAMLBUILD) ppx/ppx_deriving_data.native

showcode: all
	$(OCAMLBUILD) show_gshow_code.byte
	./show_gshow_code.byte

metasyb.cma:
	$(OCAMLBUILD) $@

metasyb_staged.cma:
	$(OCAMLBUILD) $@

install:
	ocamlfind install metaocaml-syb META \
            $$(ls _build/lib/*.cma)  \
            $$(ls _build/lib/*.cmi)  \
            $$(ls _build/lib/*.cmti) \
            $$(ls _build/lib/*.mli)  \

uninstall:
	ocamlfind remove metaocaml-syb

clean:
	$(OCAMLBUILD) -clean

.PHONY: all clean test test-build xpath syntax

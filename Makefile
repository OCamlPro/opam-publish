all: opam-publish

opam-publish: _build/src/opam_publish.native
	cp $< $@

ALWAYS:
	@
_build/src/opam_publish.%: ALWAYS
	ocamlbuild -use-ocamlfind -pkgs opam-lib,opam-lib.format,opam-lib.repository,opam-lib.state,cmdliner,github-unix src/opam_publish.$*

PREFIX ?= $(shell opam config var prefix)
install:
	@opam-installer --prefix=$(PREFIX) opam-publish.install
remove:
	@opam-installer -u --prefix=$(PREFIX) opam-publish.install

clean:
	ocamlbuild -clean


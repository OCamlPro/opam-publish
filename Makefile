all: opam-publish

opam-publish: _build/src/opam_publish.native
	cp $< $@

ALWAYS:
	@
_build/src/opam_publish.%: ALWAYS
	ocamlbuild -use-ocamlfind -pkgs opam-core,opam-format,opam-repository,opam-state,cmdliner,github.unix src/opam_publish.$*

PREFIX ?= $(shell opam config var prefix)
install:
	@opam-installer --prefix=$(PREFIX) publish.install
remove:
	@opam-installer -u --prefix=$(PREFIX) publish.install

clean:
	ocamlbuild -clean


.PHONY: install-opam-and-dune all all-w serve build-container run-container fly-deploy

all:
	dune build ./server_all.exe @runtest
all-w:
	dune build -w ./server_all.exe @runtest
serve:
	dune exec -w -- ./server_all.exe serve -p 8081

build-container:
	dune build -- ./server_all.exe
	podman build -f site/deployment/Dockerfile . -t ortografe-server

run-container: build-container
	@ # need --init otherwise we're process 1, and signals are default ignore, or something
	podman run --init -p 8082:8080 localhost/ortografe-server

fly-deploy: build-container
	fly deploy

install-opam-and-dune:
	@ # instructions from https://opam.ocaml.org/doc/Install.html
	bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
	opam install dune sexp
	opam install $(dune describe external-lib-deps | sexp select external_deps | sexp query 'each (index 0)' | sort -u | grep -v [.])

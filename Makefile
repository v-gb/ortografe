.PHONY: install-opam-and-dune all all-w serve build-container run-container fly-deploy

all:
	dune build --trace-file _build/trace ./server_all.exe @default @runtest
all-w:
	dune build --trace-file _build/trace -w ./server_all.exe @default @runtest
serve:
	dune exec --trace-file _build/trace -w -- ./server_all.exe serve -p 8081

build-container:
	dune build --trace-file _build/trace -- ./server_all.exe
	podman build -f site/deployment/Dockerfile . -t ortografe-server

run-container: build-container
	@ # need --init otherwise we're process 1, and signals are default ignore, or something
	podman run --init -p 8082:8080 localhost/ortografe-server

fly-deploy: build-container
	fly deploy

update-opam:
	@ # it's a mystery why you have to run dune describe external-lib-deps
	@ # and tell dune to put that in the opam file, rather than have dune just do it
	./update-opam-file

install-opam-and-dune:
	@ # instructions from https://opam.ocaml.org/doc/Install.html
	if ! which opam > /dev/null; then bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"; fi
	@ echo "[32mignore complaints about missing fields below, opam is being silly, and it doesn't prevent the command from working[39m"
	opam install --deps-only ./ortografe.opam

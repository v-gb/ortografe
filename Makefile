.PHONY: first-install all all-w serve build-container run-container fly-deploy tarball

all:
	opam exec -- dune build --trace-file _build/trace ./server_all.exe @default @runtest
all-w:
	opam exec -- dune build --trace-file _build/trace -w ./server_all.exe @default @runtest
serve:
	opam exec -- dune exec --trace-file _build/trace -w -- ./server_all.exe serve -p 8081

build-container:
	opam exec -- dune build --trace-file _build/trace -- ./server_all.exe
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

first-install:
	@ # instructions from https://opam.ocaml.org/doc/Install.html
	if ! which opam > /dev/null; then bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"; fi
	if ! [ -d _opam ]; then opam switch create . 5.1.1 --yes; fi
	@ echo "[32mignore complaints about missing fields below, opam is being silly, and it doesn't prevent the command from working[39m"
	opam exec -- opam install --locked --deps-only ./ortografe.opam --yes
	mkdir -p _build
	# build only the extension, as building everything implies grabbing
	# stuff from wikisource, which is a problem and is thus not automatic
	opam exec -- dune build extension/extension2.zip
	@ echo "[32mExtensions built at _build/default/extension/*.zip ![39m"

tarball:
	@ # requested by the firefox addon website, due to the generated javascript
	jj files | tar -zcf source.tar.gz --files-from=-

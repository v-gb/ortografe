.PHONY: all all-jsopt all-w all-w-jsopt serve serve-jsopt serves
all:
	opam exec -- dune build --trace-file _build/trace ./server_all.exe @default @runtest
all-jsopt:
	opam exec -- dune build --profile jsopt --trace-file _build/trace ./server_all.exe @default @runtest
all-w:
	opam exec -- dune build --trace-file _build/trace -w ./server_all.exe @default @runtest
all-w-jsopt:
	opam exec -- dune build --profile jsopt --trace-file _build/trace -w ./server_all.exe @default @runtest
serve:
	opam exec -- dune exec --trace-file _build/trace -w -- ./server_all.exe serve -p 8081
serve-jsopt:
	opam exec -- dune exec --profile jsopt --trace-file _build/trace -w -- ./server_all.exe serve -p 8081
serves:
	opam exec -- dune exec --trace-file _build/trace -w -- ./server_all.exe serve -p 8081 --tls

.PHONY: build-container build-container-jsopt run-container fly-deploy build-extension
build-container:
	opam exec -- dune build --trace-file _build/trace -- ./server_all.exe
	podman build -f site/deployment/Dockerfile . -t ortografe-server
build-container-jsopt:
	opam exec -- dune build --profile jsopt --trace-file _build/trace -- ./server_all.exe
	podman build -f site/deployment/Dockerfile . -t ortografe-server

run-container: build-container
	@ # need --init otherwise we're process 1, and signals are default ignore, or something
	podman run --init -p 8082:8080 localhost/ortografe-server

fly-deploy: build-container-jsopt
	fly deploy

build-extension: all-jsopt tarball

.PHONY: update-opam update-lock-file upgrade-opam
update-opam:
	@ # it's a mystery why you have to run dune describe external-lib-deps
	@ # and tell dune to put that in the opam file, rather than have dune just do it
	opam exec -- ./update-opam-file
update-lock-file:
	rm -f ortografe.opam.locked
	opam lock ./ortografe.opam
upgrade-opam:
	opam upgrade
	make all
	make update-opam
	make all
	make update-lock-file
	make all

.PHONY: first-install
first-install:
	@ # instructions from https://opam.ocaml.org/doc/Install.html
	if ! which opam > /dev/null; then bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"; fi
	if ! [ -d _opam ]; then opam switch create --empty .; fi
	@ # with --locked, opam sometimes uses the listed versions and sometimes not, but
	@ # using the lock file directly seems much simpler.
	opam exec -- opam install --depext-only ./ortografe.opam.locked --yes
	opam exec -- opam install --deps-only ./ortografe.opam.locked --yes
	mkdir -p _build
	make all-w
	@ echo "[32mExtensions built at _build/default/extension/*.zip ![39m"

.PHONY: tarball
tarball:
	@ # requested by the firefox addon website, due to the generated javascript
	jj file list | tar -zcf source.tar.gz --files-from=-

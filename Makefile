.PHONY: all all-w serve serves
all:
	opam exec -- dune build --trace-file _build/trace ./server_all.exe @default @runtest
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

.PHONY: build-container run-container fly-deploy
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

.PHONY: update-opam update-lock-file
update-opam:
	@ # it's a mystery why you have to run dune describe external-lib-deps
	@ # and tell dune to put that in the opam file, rather than have dune just do it
	opam exec -- ./update-opam-file
update-lock-file:
	rm -f ortografe.opam.locked
	opam lock ./ortografe.opam

.PHONY: first-install
first-install:
	./first-install # provides opam + os stuff like dot
	@ # surely opam has to provide ways to do this ??
	if ! [ -d _opam ]; then ocaml_version=$$(sed -n -e 's/.*"ocaml" {= "\(.*\)"}/\1/p'  ortografe.opam.locked); opam switch create . $$ocaml_version --locked --deps-only --yes; else opam exec -- opam install --locked --deps-only ./ortografe.opam --yes; fi
	mkdir -p _build
	make all-w
	@ echo "[32mExtensions built at _build/default/extension/*.zip ![39m"

.PHONY: tarball
tarball:
	@ # requested by the firefox addon website, due to the generated javascript
	jj files | tar -zcf source.tar.gz --files-from=-

.PHONY: serve serve-w build-container run-container fly-deploy

serve:
	dune exec -- ./server_with_deps.exe serve -p 8081
serve-w:
	dune exec -w -- ./server_with_deps.exe serve -p 8081

build-container:
	dune build -- ./server_with_deps.exe
	podman build -f site/deployment/Dockerfile . -t ortografe-server

run-container: build-container
	@ # need --init otherwise we're process 1, and signals are default ignore, or something
	podman run --init -p 8082:8080 localhost/ortografe-server

fly-deploy: build-container
	fly deploy

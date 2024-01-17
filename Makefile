.PHONY: all all-w build-container run-container fly-deploy

all:
	dune build ./server_all.exe
all-w:
	dune exec -w -- ./server_all.exe serve -p 8081

build-container:
	dune build -- ./server_all.exe
	podman build -f site/deployment/Dockerfile . -t ortografe-server

run-container: build-container
	@ # need --init otherwise we're process 1, and signals are default ignore, or something
	podman run --init -p 8082:8080 localhost/ortografe-server

fly-deploy: build-container
	fly deploy

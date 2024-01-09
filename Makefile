.PHONY: server cli serve build-container run-container fly-deploy

server cli &:
	# these depend on dict.csv, but we assume that's already built if desired
	dune build --release site/server/server.exe doc-conversion/bin/main.exe

serve: server
	_build/default/site/server/server.exe serve -p 8081

build-container: server cli
	podman build -f site/deployment/Dockerfile . -t ortografe-server

run-container: build-container
	# need --init otherwise we're process 1, and signals are default ignore, or something
	podman run --init -p 8082:8080 localhost/ortografe-server

fly-deploy: build-container
	fly deploy

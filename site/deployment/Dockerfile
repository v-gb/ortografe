FROM debian:stable-slim as build
RUN apt-get update
RUN apt-get install --no-install-recommends -y libev4 libssl3
COPY _build/default/site/server/server.exe /bin/app

# Keep in sync with server.ml
COPY _build/default/site/static/ /static

ENTRYPOINT exec /bin/app serve
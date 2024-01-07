FROM debian:stable-slim as build
RUN apt-get update
RUN apt-get install --no-install-recommends -y libev4 libssl3 bubblewrap libreoffice
# libreoffice makes the image much bigger (100MB -> 600MB or
# so). Maybe we should start with a bigger image, considering how much
# stuff that pulls in

COPY _build/default/doc-conversion/server/server.exe /bin/app
COPY index.html extension/src/rewrite.js site/page.js /static/
COPY extension/src/dict.js /static/dict_snapshot.js

ENTRYPOINT exec /bin/app serve
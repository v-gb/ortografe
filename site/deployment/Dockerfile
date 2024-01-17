FROM debian:stable-slim as build
RUN apt-get update
RUN apt-get install --no-install-recommends -y libev4 libssl3 bubblewrap libreoffice
# libreoffice makes the image much bigger (100MB -> 600MB or
# so). Maybe we should start with a bigger image, considering how much
# stuff that pulls in

COPY _build/default/site/server/server.exe /bin/app

# Keep in sync with server.ml
COPY extension/dict.gen.csv /static/erofa.csv
COPY extension/dict1990.gen.csv /static/rect1990.csv
COPY site/client/index.html site/client/page.js extension/src/rewrite.js extension/dict.gen.csv extension/src/dict.js _build/default/doc-conversion/bin/ortografe_cli.exe _build/default/erofa-texte.pdf /static/
COPY _build/default/dl-conv.gen/ /static/books/

ENTRYPOINT exec /bin/app serve
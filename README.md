This project contains tools to help with proposed spelling
simplifications for French :

- a [browser extension](extension/README.md), to browse the web with
the adjusted spelling
- a WIP [website](https://ortografe-server.fly.dev/), to transcribe text
and documents interactively without installing anything. [Source](site/).
- a [CLI tool](doc-conversion/) that transcribes text and documents in
  various formats (pure text, .html, .xhtml, .htmlz, .doc, .docx, .epub).

We may add more tools in the future.

# Building locally

The command below should:

- build the whole repository (extension, website and CLI converter)
- rebuild everything as you make changes

```bash
make install-opam-and-build
make all-w
```

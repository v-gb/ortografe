This project contains tools to help with proposed spelling simplifications for French:

- a [browser extension](extension/README.md), to browse the web with the adjusted spelling
- a WIP [website](https://ortografe-server.fly.dev/), to transcribe text and documents
interactively without installing anything. [Source](site/).
- a [CLI tool](doc-conversion/) that transcribes text and documents in various formats
  (pure text, .html, .xhtml, .htmlz, .doc, .docx, .epub).
- [data](data/) for this work (it is surprisingly difficult to find structured data)

We may add more tools in the future.

The conversion code proper could probably be adapted to work for other spelling changes,
or for other languages (the main requirements would be: text is made of words separated by
spaces, and single words can be replaced by other words without analysis of the context).

# Building locally

The command below should:

- build the whole repository (extension, website and CLI converter)
- rebuild everything as you make changes

```bash
make install-opam-and-dune
make all-w
```

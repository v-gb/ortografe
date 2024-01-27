This project contains tools to help with proposed spelling simplifications for French:

- a [browser extension](extension/README.md) that applies the spelling change to every
  visited page, without the user having to take any action like clicking a "translate"
  button. This is fast enough that you won't see the transcription happening, and it
  doesn't slow down the browing experience. The extension refrains from running on pages
  not in French, to avoid messing up English text.
  
  This extension is deployed:
    - [for firefox](https://addons.mozilla.org/fr/firefox/addon/orthographe-simplifi%C3%A9e/) (computer and phone)
    - [for chrome](https://chromewebstore.google.com/detail/orthographe-simplifiée/jdicbfmgcajnpealjodkghahiakdafcl), and presumably all chromium-based browsers. Chrome doesn't support extensions on Android, but some of the other chromium-based browsers do, so the extension should work there.

    
- a [WIP website](https://ortografe-server.fly.dev/), to transcribe text and documents
interactively without installing anything. [Source](site/).

- a [CLI tool](doc-conversion/) that transcribes text and documents in various formats
  (pure text, .html, .xhtml, .htmlz, .doc, .docx, .odt, .epub).

- [data](data/) for this work (it is surprisingly difficult to find structured data)

We may add more tools in the future.

# Different spelling changes

Almost none of the code cares about the specific spelling change. The conversion code
should work for other spelling changes or other languages, with the following constraints:

- the language has space-separated words
- the spelling change can be expressed as a word->word dictionary mapping old spelling to
  new spelling, without analysis of the context. For instance, it would be possible to
  british-ify american spellings, with a mapping theater->theatre,
  finalize->finalise, etc.

The browser extension can already load a custom directory, although it currently hardcodes
that only pages in French should be rewritten.

# Building locally

The commands below should:

- build the whole repository (extension, website and CLI converter)
- rebuild everything as you make changes

```console
$ make install-opam-and-dune
$ make all-w
```

Please open an issue if you want to build this repository, but can't!

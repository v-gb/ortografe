This project contains tools to help with proposed spelling rationalizations for French:

- a [browser extension](extension/README.md) that applies the spelling change to every
  visited page, without the user having to take any action like clicking a "translate"
  button. This is fast enough that you won't see the transcription happening, and it
  doesn't slow down the browing experience. The extension refrains from running on pages
  not in French, to avoid messing up English text.
  
  This extension is deployed:
    - [to firefox](https://addons.mozilla.org/fr/firefox/addon/orthographe-rationnelle/) (computer and phone)
    - [to chrome](https://chromewebstore.google.com/detail/orthographe-rationnelle/jdicbfmgcajnpealjodkghahiakdafcl), and presumably all chromium-based browsers. Chrome doesn't support extensions on Android, but some of the other chromium-based browsers do, so the extension should work there.
    - [to safari](https://apps.apple.com/us/app/orthographe-rationnelle/id6482850164) (MacOS and iOS)
    
- a [website](https://orthographe-rationnelle.info/) that provides an entry point to all
  the tools, and in particular transcribe text and documents interactively without
  installing anything. [Source](site/).

- a [CLI tool](doc-conversion/) that:

    - transcribes text and documents in various formats (pure text, .html, .xhtml, .htmlz, .doc, .docx, .odt, .epub)
    - creates dictionaries for other kinds of spelling changes (which can be used to convert document, or with the browser extension)

- [data](data/) for this work (it is surprisingly difficult to find structured data)

We may add more tools in the future.

# Different spelling changes

Almost none of the code cares about the specific spelling change. The conversion code
should work for other spelling changes or other languages, with the following constraints:

- the language has space-separated words
- the spelling change can be expressed as a word->word dictionary mapping old spelling to
  new spelling, without analysis of the context.

For instance, by loading a dictionary containing ([dict format
doc](extension/src/dict-format.md)):

```
{ "lang": "en" }
theater,theatre
finalize,finalise
learned,learnt
```

you would British-ify American spellings.

# Building locally

The commands below should:

- build the whole repository (extension, website, CLI converter and dictionary creation)
- run the tests
- rebuild everything as you make changes

```console
$ make first-install
$ make all-w
```

Please open an issue if you want to build this repository, but can't!

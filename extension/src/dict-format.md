% Dictionary file format

The format is :

- list of lines (separated by newlines, no carriage returns)
- each line has two values separated by a comma. No csv-style quoting is supported.
  The first value is the old spelling, the second value is the new spelling.

  Capitalized words in texts are looked up first as is in the dictionary, then
  uncapitalized. This allows common nouns to be rewritten when capitalized (in titles for
  instance), while allowing specific rules for proper nouns.

  If an entry maps to the empty string, the word is left alone (same effect as if
  you had a no-op entry like "abc,abc", but ensuring that the entry won't be highlighted
  when highlighting is enabled).

  A dash-separated word is first looked up as a whole in the dictionary, and if it's
  absent, each component is looked up and rewritten independently.

Optionally, the first line can contain a one-line json value with the following schema:

```
{
  desc : string,
  // A short description that shows up in the extension's options
  // page. If omitted, a description is created based on the
  // dictionary.

  lang : string,
  // which pages should the dictionary be used for. "fr" if omitted.
  // See https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/lang
  // for possible values

  supports_repeated_rewrites : bool,
  // Defaults to true. The extension handles dynamic pages by
  // rewriting the whole page periodically. This can cause issues for
  // dictionaries that are not idempotent, dictionaries that rewrite
  // `a` into `b`, and `b` into `c`, causing // the word `a` in the
  // page to be rewritten as `c`. Dictionaries that have this problem
  // can choose to disable rewrite, if the loss of functionality is
  // worth reducing the confusion.

  plurals_in_s : bool,
  // Defaults to true. When set, the extension assumes that if a word
  // from the page, say "pains", is missing from the dictionary but
  // "pain" is in the dictionary, then "pains" should be rewritten to
  // whatever "pain" gets rewritten to, plus a trailing "s".
}
```

Example of a dictionary that replaces `il` or `ill` by `y`:

```
{"desc":"il/y","lang":"fr","supports_repeated_rewrites":true,"plurals_in_s":true}
fille,fiye
travail,travaye
famille,famiye
travaille,travaye
travailler,travayer
```

## Asking other people to load your dictionary

If you want to ask people to load a dictionary you provide, you can of course ask them to
copy a link and paste it into the extension.

Alternatively, once someone is running the extension, the popup options page will look for
an element with class `orthographe-rationnelle-dict` and attribute
`orthographe-rationnelle-dict-url` in the current page, and propose loading that. So
putting:

    <span class=orthographe-rationnelle-dict orthographe-rationnelle-dict-url="https://blablabla"></span>

in a page would make the extension provide a potentially easier way to load the
dictionary.

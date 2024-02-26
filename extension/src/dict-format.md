# Format of the dictionary files

The format is :

- list of lines (separated by newlines, no carriage returns)
- each line has two values separated by a comma. No csv-style quoting is supported.
  The first value is the old spelling, the second value is the new spelling.

Optionally, the first line can contain a one-line json value with the following schema:

```
{
  desc : string,
  // A short description that shows up in the extension's options page. If
  // omitted, a description is created based on the dictionary.

  lang : string,
  // which pages should the dictionary be used for. "fr" if omitted.
  // See https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/lang
  // for possible values

  supports_repeated_rewrites : bool,
  // Defaults to true. The extension handles dynamic pages by rewriting the whole
  // page periodically. This can cause issues for dictionaries that are not
  // idempotent, dictionaries that rewrite `a` into `b`, and `b` into `c`, causing
  // the word `a` in the page to be rewritten as `c`. Dictionaries that have this
  // problem can choose to disable rewrite, if the loss of functionality is worth
  // reducing the confusion.

  plurals_in_s : bool,
  // Defaults to true. When set, the extension assumes that if a word from the page,
  // say "pains", is missing from the dictionary but "pain" is in the dictionary,
  // then "pains" should be rewritten to whatever "pain" gets rewritten to, plus a
  // trailing "s".
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

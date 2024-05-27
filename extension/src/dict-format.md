% Fichier de dictionnaire

**(see English version further down)**

Un fichier dictionnaire spécifie comment transcrire les mots d'un texte. Voici un
exemple de dictionnaire qui remplace quelques `ph` par des `f` :

```
{"desc":"ph/f","lang":"fr"}
photo,foto
photographie,fotografie
téléphone,téléfone
```

Le format est :

- une liste de lignes (séparées par des sauts de lignes, pas de retours chariots)
- chaque ligne contient l'ancienne orthographe, une virgule, la nouvelle
  orthographe. Il n'y a ni guillemets ni échappements comme on en trouve dans les csv.

  Un mot capitalisé dans un texte est d'abord cherché tel que dans le dictionnaire,
  puis s'il n'y est pas, décapitalisé. Ainsi les noms communs sont réécrits même s'ils
  sont capitalisés (dans les titres par exemple), mais des réécritures spécifiques pour
  les noms propres sont possibles.

  Si la nouvelle orthographe est une chaine vide, le mot n'est pas modifié.
  
  Un mot contenant des traits d'union est d'abord cherché entier dans le dictionnaire,
  mais s'il n'y est pas, alors chaque morceau est traité indépendamment.

Le fichier peut optionnellement contenir un objet json en première ligne, son schéma
est décrit dans la partie anglaise plus bas.

### Faire utiliser votre dictionnaire par d'autres gens

Voir la section dans la partie anglaise.

## English version

A dictionary file specifies how to transcribe the words of a text. Here is a dictionary
that replaces some `ph` by `f`:

```
{"desc":"ph/f","lang":"en"}
photo,foto
photography,fotografy
phone,fone
```

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

  plurals_in_s : (bool | string),
  // Defaults to true. When set, the extension assumes that if a word
  // from the page, say "pains", is missing from the dictionary but
  // "pain" is in the dictionary, then "pains" should be rewritten to
  // whatever "pain" gets rewritten to, plus a trailing "s".
  //
  // When set to a string, the behavior is the same as with true, except that
  // instead of appending a trailing "s", the string itself is appended.
  //
  // The deployed Safari extension currently only supports dictionaries
  // where this field is a boolean.
}
```


### Asking other people to load your dictionary

If you want to ask people to load a dictionary you provide, you can of course ask them to
copy a link and paste it into the extension.

Alternatively, once someone is running the extension, the popup options page will look for
an element with class `orthographe-rationnelle-dict` and attribute
`orthographe-rationnelle-dict-url` in the current page, and propose loading that. So
putting:

    <span class=orthographe-rationnelle-dict orthographe-rationnelle-dict-url="https://blablabla"></span>

in a page would make the extension provide a potentially easier way to load the
dictionary.

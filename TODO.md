Tàches possibles :

De façon générale, voir ./README.md pour comment tout construire.

## Linguistique

- plus de transformations d'orthographes
  - Utiliser `_build/default/dict-gen/bin/dict_gen.exe gen --le-nom-de-la-nouvelle-règle` pour voir le résultat.
  - s'inspirer des autres règles dans `dict-gen/common/rules.ml`
  - exemples de règles possibles à implémenter :
    - règles manquantes de l'arpetani
    - (pour voir ce que ça donne) essai de simplification des lettres muettes comme e du féminin ou s du pluriel
    - (pour voir ce que ça donne) remplacer les s prononcés z par z

- dans l'extension, permettre de créer des dictionnaires dans l'extension en
  écrivant quelque chose comme "oi -> wa".

- pluriels simples des rectifications de 1990. En gardant le mot précédent en mémoire, et
  quand on trouve un mot ambigu comme sèche-cheveux, on peut regarder si mot précédent
  est un article comme le/un/mes/quelques etc. Ce pourrait suffisamment peu couteux pour
  être utilisable dans l'extension.

## Technique

- fournir la conversion en local
  Ça permettrait de convertir des documents :
  - sans limite de taille
  - avec des dictionnaires calculés, sans ajouter de la charge au serveur
  Et de manière, ça réduit la dépendence vers un service centralisée.

  Deux options semblent possible :
  - compiler ortografe_cli.exe en javascript, et fournir ça
  - fournir un web app progressive https://developer.mozilla.org/fr/docs/Web/Progressive_web_apps
    
- extension : ajouter une façon de signaler des problèmes, comme mauvaise réécriture ?

- extension : rééssayer de compiler en wasm, pour optimiser la création de dictionnaire
  (3.5x plus lent en javascript qu'en ocaml natif, maintenant). En 2024-02,
  `wasm_of_ocaml`, il est trop tôt pour essayer, et wasm-gc à l'air lent en général,
  surtout dans firefox.

- faire que quelqu'un d'autre puisse convertir des documents avec des outils linguistiques
  arbitraires, sans avoir à faire du travail pour chaque type de document. Idée : 

        $ ortografe_cli conv -extract foo.odt | tee foo.json
        "Le titre"
        { kind: "flush" }
        "Voici un paragraphe de text qui contient un mot en "
        "gras"
        "ou un "
        "lien"
        "."
        { kind: "flush" }
        ...
        $ sed 's/ph/f/g' foo.json > foo2.json
        $ ortografe_cli conv -replace foo2.json foo.odt

    Entre chaque chaine, il y a des bouts de structure du document (comme des informations
    de formatage), c'est pour ça que la deuxième commande veut exactement le même nombre
    de chaine que la première commande a affiché.

- Meilleure détection de l'anglais. On a tendance à réécrire des mots en anglais dans des
  pages majoritairement anglaises mais avec un peu de structure de la page en français
  (exemple : l'interface de reddit sera en français, mais le contenu d'un subreddit peut
  être dans une autre langue). Deux idées :

    - calculer la langue d'une page avec du code dédié aux gros sites, où un regarde
      une partie spécifique de la page (genre description de la vidéo sur youtube mais
      la barre de droite sur reddit)
    - tout les X kB de texte, on reconsidère si on n'a pas changé de langue. Ça ne
      garanti pas l'absence de réécriture, mais elles ne sont pas si nombreuses que ça,
      donc ça pourrait suffire.

- Supporter la conversion de fichier PDF. Probablement difficile.

- Charger un dictinonaire à partir d'une URL (sur téléphone, il est plus simple que de
  télécharger un fichier puis de le charger). Encore mieux, remplir le champ par défault
  d'après le contenu de la page (par exemple, un lien avec une class
  "orthographe-rationnelle").

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

- pluriels simples des rectifications de 1990. En gardant le mot précédent en mémoire, et
  quand on trouve un mot ambigu comme sèche-cheveux, on peut regarder si mot précédent
  est un article comme le/un/mes/quelques etc. Ce pourrait suffisamment peu couteux pour
  être utilisable dans l'extension.

- dans l'extension, supporter la réécriture de "mots" comprenant des espaces. Ça
  permettrait de réécrire « un a priori » en « un apriori ». Ça permettrait de parfois
  désambigüiser des homographes comme « je sens qu'il a du bon sens ».

- investiguer le cout de supporter un peu d'analyse des phrases, pour distinguer singulier
  et pluriel (un sèche-cheveu, des sèche-cheveux), verbe et nom (sens, couvent). Ou alors
  on peut « simplement » demander à une IA de désambigüiser, pour les calculs offline.

## Technique
    
- extension : ajouter une façon de signaler des problèmes, comme mauvaise réécriture ?

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

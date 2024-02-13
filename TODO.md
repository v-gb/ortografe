Tàches possibles :

- (introduction) petite simplification

  Dans dict-gen/lib/rewrite.ml, les règles pour ortograf.net ont un cas particulier pour
  eût et conjugaison similaires, pour que ce mot soit transcrit en "u" au lieu de "eu". Je
  pense que le problème est que dict-gen/lib/rules.ml dit que "eu" peut se prononcé /y/
  mais on n'a pas l'équivalent pour "eû".

  Donc il faudrait :
  - construire le repo : `make all-w`
  - supprimer le cas particulier pour "eùt" dans `dict-gen/lib/rewrite.ml`, et constater en lançant `_build/default/dict-gen/bin/dict_gen.exe gen --ortograf.net` que eût a la mauvaise orthographe
  - confirmer l'hypothèse du mauvais découpage en graphème: `_build/default/dict-gen/bin/dict_gen.exe check | grep eût` (les barres verticales indiquent les coupures)
  - rajouter le graphème `"eû"` -> `[ "y", Surprising ]` qq part dans `dict-gen/lib/rules.ml`
  - vérifier que dans le dictionnaire ortograf.net, l'orthographe est maintenant correcte
  - s'assurer que ça n'a pas d'effet sur la réécriture érofa `diff -u data/homemade/dict-rect1990+erofa.adds.csv <(_build/default/dict-gen/bin/dict_gen.exe erofa-ext)`

- plus de transformations d'orthographes
  - même commande pour construire et exécuter le code que l'entrée du dessus. Utiliser `_build/default/dict-gen/bin/dict_gen.exe gen --le-nom-de-la-nouvelle-règle` pour voir le résultat.
  - s'inspirer des autres règles dans `dict-gen/lib/rules.ml`
  - exemples de règles possibles à implémenter :
    - règles manquantes de l'arpetani + activer la nouvelle règle dans site/static/dune pour que dict-arpetani.csv soit mis à jour
    - (pour voir ce que ça donne) essai de simplification des lettres muettes comme e du féminin ou s du pluriel
    - (pour voir ce que ça donne) remplacer les s prononcés z par z

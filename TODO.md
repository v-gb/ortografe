Tàches possibles :

- plus de transformations d'orthographes
  - même commande pour construire et exécuter le code que l'entrée du dessus. Utiliser `_build/default/dict-gen/bin/dict_gen.exe gen --le-nom-de-la-nouvelle-règle` pour voir le résultat.
  - s'inspirer des autres règles dans `dict-gen/lib/rules.ml`
  - exemples de règles possibles à implémenter :
    - règles manquantes de l'arpetani + activer la nouvelle règle dans site/static/dune pour que dict-arpetani.csv soit mis à jour
    - (pour voir ce que ça donne) essai de simplification des lettres muettes comme e du féminin ou s du pluriel
    - (pour voir ce que ça donne) remplacer les s prononcés z par z

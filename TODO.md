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

- meilleur support des dictionnaires
  Quand un dictionnaire est chargé dans l'extension (`extension/src/{options,rewrite}.js`),
  il serait utile de pouvoir spécifier plus de choses que les mots à réécrire. Exemples :
  - désactiver la réécriture des pages dynamiques. Avec ortograf.net, on réécrit "il sent" en "il sen", et si la page change, on va la réécrire en entier et changer ça en "il sene" ("sen" est peut-être un mot japonais? je sais pas)
  - configurer la langue. Pour l'instant, l'extension ne s'applique qu'aux pages en français, mais il serait facile de généraliser ça, pour supporter d'autres langues voisines.
  - désactiver la règles des pluriels qui dit "pour réécrire totos, si totos n'est pas dans le dico, on essaie de réécrire toto et de rajouter un s au résultat". Cette règle n'est pas correcte dès qu'on supprime les s finaux dans l'orthographe finale.

  Concrètement, le changement serait (ça peut-être fait en plusieurs morceaux):
  - si la première ligne des csv commence par un "{", alors parser cette ligne comme du json
  - (?) valider le json quand on charge un fichier
  - utiliser le json dans `extension/src/rewrite.js` pour désactiver la réécriture des pages
    dynamiques en modifiant la variable `options`, probablement
  - un truc similaire avec la configuration de la langue. Et même à jour le README.md à la racine
  - un truc similaire avec la règles des pluriels
  - finalement changer les dictionnaire créés par `_build/default/dict-gen/bin/dict_gen.exe gen` pour désactiver la réécriture et les pluriels pour les dicos ortograf.net et alfonic

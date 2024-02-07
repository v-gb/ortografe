Tàches possibles :

- (introduction) plus de support des rectifications de 90

  Les réécritures que l'on fait sont censées s'appliquer à la fois aux textes écrits en
  orthographe pré 90, et en orthographe post 90. Pour ça, on utilise entre autre
  data/lexique/1990.csv pour créer des dictionnaires qui ont en entrées à la fois
  l'ancienne et la nouvelle orthographe.
  
  Une faille actuelle est qu'on ne comprend pas que (par exemple) tsé-tsé devient tsétsé,
  alors qu'on comprend que auto-stop devient autostop. La raison est que tsé-tsé est
  invariable avant 90, mais devient tsétsé au singulier et tsétsés au pluriel après 90. On
  ne sais pas différencier si le mot est censé être pluriel ou singulier, donc on ne fait
  rien. Il serait plus utile d'au moins réécrire tsé-tsé en tsétsé, même si on ne sait pas
  faire les pluriels.

  Concrètement, ça devrait être un changement dans le sql vers la fin de
  extension/import-dict, pour garder convertir un mot `M` en `M.replace('-', '')`,
  pourvu que `M.replace('-', '')` soit une des orthographes possibles après 90. Les
  mots qui deviennent variable ont `type=2`.
  
  Puis lancer `import-dict`, constater si extension/dict1990.gen.csv change comme on l'attend.

  Si oui, alors tu peux lancer `(echo old,new; _build/default/dict-gen/bin/dict_gen.exe erofa-ext) > data/homemade/dict-rect1990+erofa.adds.csv` pour mettre à jour l'extension du ditionnaire érofa, relancer `import-dict` pour incorporer ces changements dans l'extension, et puis vérifier dans un navigateur que les choses marchent (voir `extension/README.md` pour comment charger l'extension).

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

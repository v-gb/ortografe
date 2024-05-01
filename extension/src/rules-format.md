# Format des règles

La zone de texte permet de spécifier des règles de réécriture et d'en générer des dictionnaires. Par exemple la règle de réécriture **eau/ô** veut dire « dans tous les mots (connus), remplace *eau* par *ô*, pourvu que la prononciation soit préservée&nbsp;». Cette règle transformera donc *château* en *châtô*, *caniveau* en *canivô*, etc.

Mais elle ne touchera pas *souriceau* car *souricô* ne se prononcerait plus pareil. Une meilleure règle serait donc **eau/ô ceau/çô**, pour réécrire *souriceau* en *souriçô* en plus des changements précédents.

Les changements sont appliqués dans l'ordre écrit. Dans l'exemple précédent, l'ordre n'importe pas, mais si on voulait changer tous les *au* en *ô*, **au/ô eau/ô** réécrirait bien *miaule* en *miôle*, mais aussi *château* en *châteô*, car la règle **au/ô** s'applique d'abord. Dans l'ordre inverse, **eau/ô au/ô** réécrirait *château* en *châtô* et *miaule* en *miôle*.

Finalement, un cas spécial est que les règles **ell/El** (ou **ett/Et** et ainsi de suite avec la plupart des consonnes) permettent de réécrire *ell* en soit *él* soit *èl* suivant l'ouverture de la syllabe du *e*. Cela produit généralement des meilleurs résultats que des règles **ell/él ell/èl** (çàd choisir l'accent en fonction de la prononciation), car ces prononciations dans les lexiques sont souvent discutables.

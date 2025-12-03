# Valider un fichier de données à importer

``` r
library(didoscalim)
#> ✖ Environnement DiDo actif : DEV
```

## Valider un fichier

Ce document a pour objectif de vous aider à valider un fichier de
données.

Votre fichier peut contenir des centaines de milliers de lignes et des
dizaines de colonnes dont de type `COG`. Avant tout chargement dans
l’environnement de production, il est conseillé de le valider en le
chargeant dans l’environnement ECOLE de DiDo.

La fonction
[`check_import_file()`](https://mtes-mct.github.io/didoscalim/reference/check_import_file.md)
automatise cela :

``` r
check_import_file(dido_example("file-with-error.csv"))
#> ✖ Problème sur le fichier `/home/nc/R/x86_64-pc-linux-gnu-library/4.3/didoscalim/extdata/file-with-error.csv`
#> ✖ Le fichier est invalide
#> ✖ colonne_1: La description est requise
#> ✖ colonne_2: Le type anneee n'est pas un type connu.
#> [1] FALSE
```

Cette fonction :

1.  chargera le fichier
2.  vous imprimera les erreurs
3.  supprimera les données

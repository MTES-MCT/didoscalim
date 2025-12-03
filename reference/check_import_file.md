# Vérifie un fichier CSV

Cette fonction permet de vérifier rapidement la validité d'un fichier de
données. Elle :

1.  créé un jeu de données avec un nom aléatoire

2.  ajoute le fichier de données

3.  supprime le jeu de données

4.  affiche les erreurs rencontrées s'il y en a

**L'import de test ne se fera jamais sur l'environnement de production**
mais sur un des environnments DEV, PREPROD ou ECOLE. Si aucun de ces
environnements n'est configuré, la fonction remonte une erreur

## Utilisation

``` r
check_import_file(file_name)
```

## Arguments

- file_name:

  le fichier a valider

## Valeur de retour

TRUE si l'import s'est bien passé, FALSE sinon

## Exemples

``` r
check_import_file(dido_example("file-with-error.csv"))
#> ✖ Problème sur le fichier `/home/nc/travail/R/didoscalim/inst/extdata/file-with-error.csv`
#> ✖ Le fichier csv n'est pas valide: 400
#> ℹ url: http://alim.dido.fr/api/v1/datasets/693074dac9f6b744c79ebb99/datafiles
#> [1] FALSE
```

# Bloquer les créations de datasets, datafiles et attachments

`didoscalim_update_only()` permet d'autoriser ou d'interdire la création
d'objets dans les fonctions
[`add_or_update_dataset()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_dataset.md),
[`add_or_update_datafile()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_datafile.md)
et
[`add_or_update_attachment()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_attachment.md)
dans la suite du programme

## Utilisation

``` r
didoscalim_update_only(bool)
```

## Arguments

- bool:

  TRUE/FALSE, si TRUE bloque les créations

## Valeur de retour

Le précédente valeur ou FALSE si non fixée

## Exemples

``` r
# bloque la création dans la suite du programme
didoscalim_update_only(TRUE)
#> ℹ Création de nouveaux objets : interdite

# autorise la création dans la suite du programme
didoscalim_update_only(FALSE)
#> ℹ Création de nouveaux objets : autorisée
```

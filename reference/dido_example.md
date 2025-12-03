# Retourne le chemin du fichier exemple données ou le chemin des fichiers exemple

Les fichiers exemples sont stockés dans `inst/extdata`

## Utilisation

``` r
dido_example(file = NULL)
```

## Arguments

- file:

  le nom du fichier. Si `NULL` les exemples sont listés.

## Exemples

``` r
dido_example()
#> [1] "attachment.txt"      "augmente.csv"        "csv-win-char.csv"   
#> [4] "exemple.csv"         "file-with-error.csv"
dido_example("exemple.csv")
#> [1] "/home/nc/travail/R/didoscalim/inst/extdata/exemple.csv"
```

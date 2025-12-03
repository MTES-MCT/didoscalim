# Retourne le dataset id de l'objet

Retourne le dataset id de l'objet

## Utilisation

``` r
get_dataset_id(data)
```

## Arguments

- data:

  un objet dido_dataset, dido_datafile ou dido_job

## Valeur de retour

le dataset id

## Exemples

``` r
ds <- list_datasets()[1, ]
get_dataset_id(ds)
#> [1] "693074aecffe21e55a9ebb97"
```

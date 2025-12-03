# Retourne le datafile rid de l'objet

Retourne le datafile rid de l'objet

## Utilisation

``` r
get_datafile_rid(data)
```

## Arguments

- data:

  un objet dido_datafile ou dido_job

## Valeur de retour

le datafile id

## Exemples

``` r
df <- list_datafiles()[1, ]
get_datafile_rid(df)
#> [1] "f0095044-65d0-4db7-a1b9-d56509b76757"
```

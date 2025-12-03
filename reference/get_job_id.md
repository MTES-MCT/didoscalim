# Retourne l'attachment id de l'objet

Retourne l'attachment id de l'objet

## Utilisation

``` r
get_job_id(data)
```

## Arguments

- data:

  un objet dido_attachment ou une chaine

## Valeur de retour

le rid de l'attachment

## Exemples

``` r
j <- list_jobs()[1, ]
get_job_id(j)
#> [1] 1
```

# Retourne l'attachment id de l'objet

Retourne l'attachment id de l'objet

## Utilisation

``` r
get_attachment_rid(data)
```

## Arguments

- data:

  un objet dido_attachment ou une chaine

## Valeur de retour

le rid de l'attachment

## Exemples

``` r
at <- list_attachments()[1, ]
get_attachment_rid(at)
#> [1] "4cea1036-175d-406b-b2c2-d56247ce77b5"
```

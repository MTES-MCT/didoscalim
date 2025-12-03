# Récupère l'ID de l'organisation de l'utilisateur.

Si l'utilisateur appartient à plusieurs organisations

## Utilisation

``` r
my_organization(name = NULL)
```

## Arguments

- name:

  name or acronym for organization, default to NULL

## Valeur de retour

un id d'organisation

## Exemples

``` r
organization_id <- my_organization()
organization_id <- my_organization(name = "BSI")
#> Error in my_organization(name = "BSI"): La recherche ne retourne aucune organisation
```

# Liste les millesimes

Retourne un tibble de tous les millésimes ou d'un datafile en
particulier

## Utilisation

``` r
list_millesimes(data = NULL)
```

## Arguments

- data:

  le résultat d'une recherche par
  [`list_datafiles()`](https://mtes-mct.github.io/didoscalim/reference/list_datafiles.md),
  un des objets
  [`dido_job()`](https://mtes-mct.github.io/didoscalim/reference/dido_job.md)
  [`dido_datafile()`](https://mtes-mct.github.io/didoscalim/reference/dido_datafile.md)
  oule rid d'un datafile

## Valeur de retour

un tibble avec les millesimes du datafile

## Voir également

Other millesime:
[`add_millesime()`](https://mtes-mct.github.io/didoscalim/reference/add_millesime.md),
[`delete_millesime()`](https://mtes-mct.github.io/didoscalim/reference/delete_millesime.md),
[`replace_millesime()`](https://mtes-mct.github.io/didoscalim/reference/replace_millesime.md)

## Exemples

``` r
library(dplyr, warn.conflicts = FALSE)

datafile <- list_datafiles() %>%
  filter(title == "Un fichier de données de test")

list_millesimes(datafile)
#> Error in abort_if_not_one_line("data"): ✖ L'argument `data` doit contenir une ligne.
#> ℹ `data` contient 0 ligne(s).
#> ℹ Avez-vous oublié de filtrer (avec dplyr::filter par exemple`) le dataframe avant de le passer en
#> argument ?
```

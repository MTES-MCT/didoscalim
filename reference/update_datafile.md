# Mettre à jour un les métadonnées d'un datafile.

Mettre à jour un les métadonnées d'un datafile.

## Utilisation

``` r
update_datafile(datafile)
```

## Arguments

- datafile:

  un objet datafile retourné par
  [`get_datafile()`](https://mtes-mct.github.io/didoscalim/reference/get_datafile.md)
  modifié par l'utilisateur

## Valeur de retour

un objet
[`dido_job()`](https://mtes-mct.github.io/didoscalim/reference/dido_job.md)
invisible

## Voir également

Other datafile:
[`add_datafile()`](https://mtes-mct.github.io/didoscalim/reference/add_datafile.md),
[`add_or_update_datafile()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_datafile.md),
[`delete_datafile()`](https://mtes-mct.github.io/didoscalim/reference/delete_datafile.md),
[`dido_datafile()`](https://mtes-mct.github.io/didoscalim/reference/dido_datafile.md),
[`get_datafile()`](https://mtes-mct.github.io/didoscalim/reference/get_datafile.md),
[`list_datafiles()`](https://mtes-mct.github.io/didoscalim/reference/list_datafiles.md)

## Exemples

``` r
library(dplyr, warn.conflicts = FALSE)

datafile <- list_datafiles() %>%
  filter(title == "Un fichier de données de test") %>%
  get_datafile() %>%
  clean_metadata()
#> Error in abort_if_not_one_line("data"): ✖ L'argument `data` doit contenir une ligne.
#> ℹ `data` contient 0 ligne(s).
#> ℹ Avez-vous oublié de filtrer (avec dplyr::filter par exemple`) le dataframe avant de le passer en
#> argument ?

datafile$temporal_coverage$start <- "2022-01-01"
datafile$temporal_coverage$end <- "2023-12-31"

update_datafile(datafile)
#> Error in UseMethod("internal_clean_metadata"): pas de méthode pour 'internal_clean_metadata' applicable pour un objet de classe "c('dido_job', 'list')"
```

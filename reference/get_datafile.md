# Récupère un datafile

Récupère les métadonnées d'un datafile

## Utilisation

``` r
get_datafile(data = NULL, dataset = NULL)
```

## Arguments

- data:

  le résultat d'une recherche par
  [`list_datafiles()`](https://mtes-mct.github.io/didoscalim/reference/list_datafiles.md),
  un des objets
  [`dido_job()`](https://mtes-mct.github.io/didoscalim/reference/dido_job.md)
  [`dido_datafile()`](https://mtes-mct.github.io/didoscalim/reference/dido_datafile.md)
  oule rid d'un datafile

- dataset:

  optionnel l'identifiant d'un dataset ou un objet
  [`dido_dataset()`](https://mtes-mct.github.io/didoscalim/reference/dido_dataset.md)

## Valeur de retour

un objet
[dido_datafile](https://mtes-mct.github.io/didoscalim/reference/dido_datafile.md)

## Détails

Lève une exception si
[`list_datasets()`](https://mtes-mct.github.io/didoscalim/reference/list_datasets.md)
fourni en entrée retourne plus d'un dataset.

## Voir également

Other datafile:
[`add_datafile()`](https://mtes-mct.github.io/didoscalim/reference/add_datafile.md),
[`add_or_update_datafile()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_datafile.md),
[`delete_datafile()`](https://mtes-mct.github.io/didoscalim/reference/delete_datafile.md),
[`dido_datafile()`](https://mtes-mct.github.io/didoscalim/reference/dido_datafile.md),
[`list_datafiles()`](https://mtes-mct.github.io/didoscalim/reference/list_datafiles.md),
[`update_datafile()`](https://mtes-mct.github.io/didoscalim/reference/update_datafile.md)

## Exemples

``` r
library(dplyr, warn.conflicts = FALSE)

datafile <- list_datafiles() %>%
  slice(1) %>%
  get_datafile(datafile)
```

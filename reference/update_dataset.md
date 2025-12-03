# Met à jour les metadonnées d'un dataset

Cette fonction permet de mettre à jour les métadonnées d'un dataset
comme, par exemple, la date de fin de couverture temporelle.

## Utilisation

``` r
update_dataset(dataset)
```

## Arguments

- dataset:

  un objet dido_dataset modifié

## Valeur de retour

un objet
[`dido_dataset()`](https://mtes-mct.github.io/didoscalim/reference/dido_dataset.md)

## Voir également

Other dataset:
[`add_dataset()`](https://mtes-mct.github.io/didoscalim/reference/add_dataset.md),
[`add_or_update_dataset()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_dataset.md),
[`dido_dataset()`](https://mtes-mct.github.io/didoscalim/reference/dido_dataset.md),
[`get_dataset()`](https://mtes-mct.github.io/didoscalim/reference/get_dataset.md)

## Exemples

``` r
library(dplyr, warn.conflicts = FALSE)

dataset <- list_datasets() %>%
  filter(title == "Un jeu de données de test") %>%
  get_dataset() %>%
  clean_metadata()

dataset$temporal_coverage$start <- "2020-01-01"
dataset$temporal_coverage$end <- "2020-12-31"
update_dataset(dataset)

update_dataset(dataset)
```

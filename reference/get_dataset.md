# Récupère les métadonnées d'un dataset

Permet de récupérer les métadonnées d'un dataset en utilisant la sortie
de la fonction
[`list_datasets()`](https://mtes-mct.github.io/didoscalim/reference/list_datasets.md)
ou un des objets
[`dido_datafile()`](https://mtes-mct.github.io/didoscalim/reference/dido_datafile.md),
[`dido_dataset()`](https://mtes-mct.github.io/didoscalim/reference/dido_dataset.md),
[`dido_job()`](https://mtes-mct.github.io/didoscalim/reference/dido_job.md)

## Utilisation

``` r
get_dataset(data)
```

## Arguments

- data:

  la sortie d'une commande
  [`list_datasets()`](https://mtes-mct.github.io/didoscalim/reference/list_datasets.md),
  un objet
  [`dido_dataset()`](https://mtes-mct.github.io/didoscalim/reference/dido_dataset.md),
  [`dido_job()`](https://mtes-mct.github.io/didoscalim/reference/dido_job.md)
  ou
  [`dido_datafile()`](https://mtes-mct.github.io/didoscalim/reference/dido_datafile.md)
  ou l'identifiant d'un dataset

## Valeur de retour

un objet
[`dido_dataset()`](https://mtes-mct.github.io/didoscalim/reference/dido_dataset.md)

## Détails

Lève une exception si
[`list_datasets()`](https://mtes-mct.github.io/didoscalim/reference/list_datasets.md)
fourni en entrée retourne plus d'un dataset.

## Voir également

Other dataset:
[`add_dataset()`](https://mtes-mct.github.io/didoscalim/reference/add_dataset.md),
[`add_or_update_dataset()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_dataset.md),
[`dido_dataset()`](https://mtes-mct.github.io/didoscalim/reference/dido_dataset.md),
[`update_dataset()`](https://mtes-mct.github.io/didoscalim/reference/update_dataset.md)

## Exemples

``` r
library(dplyr, warn.conflicts = FALSE)

ds <- list_datasets() %>%
  slice(1) %>%
  get_dataset()
```

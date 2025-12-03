# Ajoute un datafile à un dataset

Ajoute un datafile à un dataset

## Utilisation

``` r
add_datafile(
  dataset,
  title,
  description,
  file_name,
  millesime = format(Sys.time(), "%Y-%m"),
  published = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  temporal_coverage_start = NULL,
  temporal_coverage_end = NULL,
  legal_notice = "SDES",
  date_diffusion = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)
```

## Arguments

- dataset:

  l'id d'un dataset, un objet
  [`dido_dataset()`](https://mtes-mct.github.io/didoscalim/reference/dido_dataset.md),
  [`dido_datafile()`](https://mtes-mct.github.io/didoscalim/reference/dido_datafile.md)
  ou
  [`dido_job()`](https://mtes-mct.github.io/didoscalim/reference/dido_job.md)

- title:

  le titre du datafile

- description:

  la description du datafile

- file_name:

  le nom du fichier à charger

- millesime:

  le millesime (AAAA-MM). Par défaut AAAA-MM avec l'année courante et le
  mois courant

- published:

  la date/heure de publication du fichier, si non précisée, prend la
  date/heure du moment.

  Ce paramètre est au format "AAAA-MM-JJ HH:MM:SS" (ou ISO8601 si vous
  préférez). Si la timezone n'est pas précisée, la timezone de
  l'ordinateur local est utilisée.

- temporal_coverage_start:

  optionnel, la date de début de couverture du fichier de données au
  format AAAA-MM-JJ

- temporal_coverage_end:

  optionnel, la date de fin de couverture du fichier de données au
  format AAAA-MM-JJ

- legal_notice:

  les mentions légales, par défaut "SDES"

- date_diffusion:

  (optionnel) les date et heure auxquelles le fichier devra être rendu
  accessible à la diffusion.

  Ce paramètre est au format "AAAA-MM-JJ HH:MM:SS" (ou ISO8601 si vous
  préférez). Si la timezone n'est pas précisée, la timezone de
  l'ordinateur local est utilisée.

  Si cette date/heure est dans le passé, les données sont immédiatement
  accessibles, si elle est dans le futur, les données ne seront
  accessibles qu'à cette date/heure.

  Si ce paramètre est non précisé prend la date/heure courante, les
  données sont donc immédiatement accessibles.

## Valeur de retour

un objet
[`dido_job()`](https://mtes-mct.github.io/didoscalim/reference/dido_job.md)

## Voir également

Other datafile:
[`add_or_update_datafile()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_datafile.md),
[`delete_datafile()`](https://mtes-mct.github.io/didoscalim/reference/delete_datafile.md),
[`dido_datafile()`](https://mtes-mct.github.io/didoscalim/reference/dido_datafile.md),
[`get_datafile()`](https://mtes-mct.github.io/didoscalim/reference/get_datafile.md),
[`list_datafiles()`](https://mtes-mct.github.io/didoscalim/reference/list_datafiles.md),
[`update_datafile()`](https://mtes-mct.github.io/didoscalim/reference/update_datafile.md)

## Exemples

``` r
library(dplyr, warn.conflicts = FALSE)

dataset <- add_or_update_dataset(
  title = "Un dataset pour les datafiles",
  description = "Description des données statistiques",
  topic = "Transports",
  frequency = "unknown",
)
#> dataset `Un dataset pour les datafiles` créé

add_datafile(
  dataset = dataset,
  title = "titre",
  description = "description",
  file_name = dido_example("augmente.csv")
)
#>     intégration du fichier `/home/nc/travail/R/didoscalim/inst/extdata/augmente.csv`
#>  * fichier versé
#>  * fichier intégré:   rid: 4b1b9ba5-528e-485b-b5fe-8644a96051d1   millesime: 2025-12  lignes: 5

# publier un fichier de données avec toutes les métadonnées et un embargo
add_datafile(
  dataset = dataset,
  title = "titre 2",
  description = "description 2",
  file_name = dido_example("augmente.csv"),
  temporal_coverage_start = "2021-01-01",
  temporal_coverage_end = "2021-12-31",
  legal_notice = "something",
  millesime = "2020-10",
  date_diffusion = "2020-11-01 07:45:00"
)
#>     intégration du fichier `/home/nc/travail/R/didoscalim/inst/extdata/augmente.csv`
#>  * fichier versé
#>  * fichier intégré:   rid: 7215a8bc-1456-4479-abd3-1211a27a84e3   millesime: 2020-10  lignes: 5
```

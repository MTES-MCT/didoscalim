# Ajoute un fichier annexe à un dataset

Ajoute un fichier annexe à un dataset

## Utilisation

``` r
add_attachment(
  dataset,
  title,
  description,
  file_name = NULL,
  remote_url = NULL,
  type = "documentation",
  published = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)
```

## Arguments

- dataset:

  l'id d'un dataset de rattachement, un objet
  [`dido_dataset()`](https://mtes-mct.github.io/didoscalim/reference/dido_dataset.md),
  [`dido_datafile()`](https://mtes-mct.github.io/didoscalim/reference/dido_datafile.md)
  ou
  [`dido_job()`](https://mtes-mct.github.io/didoscalim/reference/dido_job.md)

- title:

  le titre du fichier annexe

- description:

  la description du fichier annexe

- file_name, remote_url:

  le nom du fichier à verser ou l'url de l'attachement.

- type:

  le type de fichier versé. Peut-être `documentation` ou
  `historical_data`. Par défaut `documentation`

- published:

  la date/heure de publication du fichier, si non précisée, prend la
  date/heure du moment.

  Ce paramètre est au format "AAAA-MM-JJ HH:MM:SS" (ou ISO8601 si vous
  préférez). Si la timezone n'est pas précisée, la timezone de
  l'ordinateur local est utilisée.

## Valeur de retour

un objet
[`dido_attachment()`](https://mtes-mct.github.io/didoscalim/reference/dido_attachment.md)

## Voir également

Other attachment:
[`add_or_update_attachment()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_attachment.md),
[`delete_attachment()`](https://mtes-mct.github.io/didoscalim/reference/delete_attachment.md),
[`dido_attachment()`](https://mtes-mct.github.io/didoscalim/reference/dido_attachment.md),
[`get_attachment()`](https://mtes-mct.github.io/didoscalim/reference/get_attachment.md),
[`list_attachments()`](https://mtes-mct.github.io/didoscalim/reference/list_attachments.md),
[`replace_attachment()`](https://mtes-mct.github.io/didoscalim/reference/replace_attachment.md),
[`update_attachment()`](https://mtes-mct.github.io/didoscalim/reference/update_attachment.md)

## Exemples

``` r
library(dplyr, warn.conflicts = FALSE)

dataset <- add_or_update_dataset(
  title = "Des données statistiques",
  description = "Description des données statistiques",
  topic = "Transports",
  frequency = "unknown",
)
#> dataset `Des données statistiques` créé

dataset %>% add_attachment(
  title = "title",
  description = "description",
  file_name = dido_example("attachment.txt"),
  type = 'historical_data'
)
#>     intégration du fichier annexe `attachment.txt`
#>  * fichier versé
#>  * fichier annexe intégré (rid: 02909c96-8601-4104-aff2-8248c982e5cf)

dataset %>% add_attachment(
  title = "title",
  description = "Un attachment sous forme de lien externe",
  remote_url = "https://www.lemonde.fr/"
)
#> Error in dido_api(method = "POST", path = url, body = jsonlite::toJSON(payload,     pretty = TRUE, auto_unbox = TRUE, na = "null")): ✖ Erreur: Un fichier joint existe déjà avec le titre title dans ce jeu de données: 400
#> ℹ url: http://alim.dido.fr/api/v1/datasets/693074b7cffe21e55a9ebb98/attachments
```

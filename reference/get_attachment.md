# Récupère les métadonnées d'un attachment

Permet de récupérer les données d'un attachment

## Utilisation

``` r
get_attachment(data = NULL, dataset = NULL)
```

## Arguments

- data:

  le résultat d'une recherche par
  [`list_attachments()`](https://mtes-mct.github.io/didoscalim/reference/list_attachments.md),
  un rid de attachment, un objet
  [`dido_attachment()`](https://mtes-mct.github.io/didoscalim/reference/dido_attachment.md)
  ou
  [`dido_job()`](https://mtes-mct.github.io/didoscalim/reference/dido_job.md)

- dataset:

  optionnel, le dataset parent de l'attachement.

## Valeur de retour

un objet
[`dido_attachment()`](https://mtes-mct.github.io/didoscalim/reference/dido_attachment.md)

## Voir également

Other attachment:
[`add_attachment()`](https://mtes-mct.github.io/didoscalim/reference/add_attachment.md),
[`add_or_update_attachment()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_attachment.md),
[`delete_attachment()`](https://mtes-mct.github.io/didoscalim/reference/delete_attachment.md),
[`dido_attachment()`](https://mtes-mct.github.io/didoscalim/reference/dido_attachment.md),
[`list_attachments()`](https://mtes-mct.github.io/didoscalim/reference/list_attachments.md),
[`replace_attachment()`](https://mtes-mct.github.io/didoscalim/reference/replace_attachment.md),
[`update_attachment()`](https://mtes-mct.github.io/didoscalim/reference/update_attachment.md)

## Exemples

``` r
library(dplyr, warn.conflicts = FALSE)

dataset <- add_or_update_dataset(
  title = "Un dataset pour les attachement",
  description = "Description des données statistiques",
  topic = "Transports",
  frequency = "unknown",
)

add_or_update_attachment(
  dataset = dataset,
  title = "title",
  description = "description",
  file_name = dido_example("attachment.txt")
)

dataset %>%
  list_attachments() %>%
  slice(1) %>%
  get_attachment()
#> List of 10
#>  $ rid          : chr "9884defa-524b-4161-86f0-9ced1f949208"
#>  $ type         : chr "documentation"
#>  $ title        : chr "title"
#>  $ description  : chr "description"
#>  $ published    : chr "2025-12-03T17:35:10.000Z"
#>  $ url          : chr "http://api.diffusion.dido.fr/files/9884defa-524b-4161-86f0-9ced1f949208"
#>  $ created_at   : chr "2025-12-03T17:35:10.291Z"
#>  $ last_modified: chr "2025-12-03T17:35:25.721Z"
#>  $ remote       : logi FALSE
#>  $ udataUrl     : chr "http://udata.dido.fr/s/resources/un-dataset-pour-les-attachement/20251203-173525/attachment.txt"
#>  - attr(*, "id")= chr "693074ce31b56d550d9ebb98"
#>  - attr(*, "class")= chr [1:2] "dido_attachment" "list"
```

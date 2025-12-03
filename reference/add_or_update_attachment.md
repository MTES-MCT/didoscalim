# Ajoute ou modifie un fichier annexe dans un dataset

met à jour l'attachement avec le même titre s'il existe sinon ajoute un
attachement.

Si vous devez mettre à jour des attachments, il est conseillé d'utiliser
la fonction
[`didoscalim_update_only()`](https://mtes-mct.github.io/didoscalim/reference/didoscalim_update_only.md),
cf
[`vignette("complements-sur-les-mises-a-jour")`](https://mtes-mct.github.io/didoscalim/articles/complements-sur-les-mises-a-jour.md)

## Utilisation

``` r
add_or_update_attachment(
  dataset,
  title,
  description,
  file_name = NULL,
  remote_url = NULL,
  published = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  check_file_date = FALSE,
  type = "documentation"
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

- published:

  la date/heure de publication du fichier, si non précisée, prend la
  date/heure du moment.

  Ce paramètre est au format "AAAA-MM-JJ HH:MM:SS" (ou ISO8601 si vous
  préférez). Si la timezone n'est pas précisée, la timezone de
  l'ordinateur local est utilisée.

- check_file_date:

  TRUE/FALSE, Si TRUE met à jour l'attachment uniquement si le fichier
  est plus récent que le last_modified de l'attachment

- type:

  le type de fichier versé. Peut-être `documentation` ou
  `historical_data`. Par défaut `documentation`

## Valeur de retour

un objet
[`dido_attachment()`](https://mtes-mct.github.io/didoscalim/reference/dido_attachment.md)
ou `NULL` si aucune création/mise à jour n'a eu lieu

## Voir également

Other attachment:
[`add_attachment()`](https://mtes-mct.github.io/didoscalim/reference/add_attachment.md),
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
  title = "Un dataset pour les attachement",
  description = "Description des données statistiques",
  topic = "Transports",
  frequency = "unknown",
)
#> dataset `Un dataset pour les attachement` créé

add_or_update_attachment(
  dataset = dataset,
  title = "title",
  description = "description",
  file_name = dido_example("attachment.txt")
)
#>     intégration du fichier annexe `attachment.txt`
#>  * fichier versé
#>  * fichier annexe intégré (rid: 9884defa-524b-4161-86f0-9ced1f949208)

add_or_update_attachment(
  dataset = dataset,
  title = "title",
  description = "description",
  remote_url = "https://www.lemonde.fr"
)
#>  * fichier annexe remplacé (rid: 9884defa-524b-4161-86f0-9ced1f949208)
#> attachment "title" modifié
```

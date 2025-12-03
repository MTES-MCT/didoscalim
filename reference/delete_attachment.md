# Supprime un attachment

Supprime un attachment

## Utilisation

``` r
delete_attachment(attachment)
```

## Arguments

- attachment:

  un objet attachment retourné par
  [`get_attachment()`](https://mtes-mct.github.io/didoscalim/reference/get_attachment.md),
  [`add_attachment()`](https://mtes-mct.github.io/didoscalim/reference/add_attachment.md)

## Valeur de retour

TRUE

## Voir également

Other attachment:
[`add_attachment()`](https://mtes-mct.github.io/didoscalim/reference/add_attachment.md),
[`add_or_update_attachment()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_attachment.md),
[`dido_attachment()`](https://mtes-mct.github.io/didoscalim/reference/dido_attachment.md),
[`get_attachment()`](https://mtes-mct.github.io/didoscalim/reference/get_attachment.md),
[`list_attachments()`](https://mtes-mct.github.io/didoscalim/reference/list_attachments.md),
[`replace_attachment()`](https://mtes-mct.github.io/didoscalim/reference/replace_attachment.md),
[`update_attachment()`](https://mtes-mct.github.io/didoscalim/reference/update_attachment.md)

## Exemples

``` r
library(dplyr, warn.conflicts = FALSE)

dataset <- add_or_update_dataset(
  title = "Un dataset pour les attachements",
  description = "Description des données statistiques",
  topic = "Transports",
  frequency = "unknown",
)
#> dataset `Un dataset pour les attachements` créé

attachment <- add_attachment(
  dataset = dataset,
  title = "title",
  description = "description",
  file_name = dido_example("attachment.txt")
)
#>     intégration du fichier annexe `attachment.txt`
#>  * fichier versé
#>  * fichier annexe intégré (rid: d34d0d19-161e-4ff9-96e2-34536d272828)
delete_attachment(attachment)
```

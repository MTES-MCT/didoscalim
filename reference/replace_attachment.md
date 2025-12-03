# Remplace un fichier annexe

Remplace un fichier annexe

## Utilisation

``` r
replace_attachment(attachment, file_name = NULL, remote_url = NULL)
```

## Arguments

- attachment:

  un objet
  [`dido_attachment()`](https://mtes-mct.github.io/didoscalim/reference/dido_attachment.md)
  ou un rid

- file_name, remote_url:

  le nom du fichier à verser ou l'url de l'attachement.

## Valeur de retour

un objet
[`dido_attachment()`](https://mtes-mct.github.io/didoscalim/reference/dido_attachment.md)

## Voir également

Other attachment:
[`add_attachment()`](https://mtes-mct.github.io/didoscalim/reference/add_attachment.md),
[`add_or_update_attachment()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_attachment.md),
[`delete_attachment()`](https://mtes-mct.github.io/didoscalim/reference/delete_attachment.md),
[`dido_attachment()`](https://mtes-mct.github.io/didoscalim/reference/dido_attachment.md),
[`get_attachment()`](https://mtes-mct.github.io/didoscalim/reference/get_attachment.md),
[`list_attachments()`](https://mtes-mct.github.io/didoscalim/reference/list_attachments.md),
[`update_attachment()`](https://mtes-mct.github.io/didoscalim/reference/update_attachment.md)

## Exemples

``` r
library(dplyr, warn.conflicts = FALSE)

dataset <- list_datasets() %>%
  filter(title == "Un jeu de données de test")

attachment <- list_attachments(dataset) %>%
  filter(title == "Un autre fichier annexe")

replace_attachment(
  attachment = attachment,
  file_name = dido_example("attachment.txt")
)
```

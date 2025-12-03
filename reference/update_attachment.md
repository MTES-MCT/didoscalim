# Mettre à jour une pièce jointe.

Mettre à jour une pièce jointe.

## Utilisation

``` r
update_attachment(attachment)
```

## Arguments

- attachment:

  un objet attachment retourné par
  [`get_attachment()`](https://mtes-mct.github.io/didoscalim/reference/get_attachment.md)
  modifié par l'utilisateur

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
[`replace_attachment()`](https://mtes-mct.github.io/didoscalim/reference/replace_attachment.md)

## Exemples

``` r
att <- list_attachments()[1, ]
dataset <- att$id
attachment <- get_attachment(list_attachments()[1, ], dataset = dataset)
attachment$title <- "un nouveau titre"
update_attachment(attachment)
```

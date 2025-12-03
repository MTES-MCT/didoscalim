# Liste les attachments

Retourne la liste des attachments ou si un objet dataset est passé en
argument, la liste des attachments de ce dataset.

## Utilisation

``` r
list_attachments(dataset = NULL)
```

## Arguments

- dataset:

  optionnel, un objet dataset

## Valeur de retour

un tibble avec les attachments

## Voir également

Other attachment:
[`add_attachment()`](https://mtes-mct.github.io/didoscalim/reference/add_attachment.md),
[`add_or_update_attachment()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_attachment.md),
[`delete_attachment()`](https://mtes-mct.github.io/didoscalim/reference/delete_attachment.md),
[`dido_attachment()`](https://mtes-mct.github.io/didoscalim/reference/dido_attachment.md),
[`get_attachment()`](https://mtes-mct.github.io/didoscalim/reference/get_attachment.md),
[`replace_attachment()`](https://mtes-mct.github.io/didoscalim/reference/replace_attachment.md),
[`update_attachment()`](https://mtes-mct.github.io/didoscalim/reference/update_attachment.md)

## Exemples

``` r
# toutes les pièces jointes
list_attachments()
#> # A tibble: 4 × 11
#>   id      rid   type  title description published url   created_at last_modified
#>   <chr>   <chr> <chr> <chr> <chr>       <chr>     <chr> <chr>      <chr>        
#> 1 693074… 4cea… docu… Un a… Un fichier… 2025-12-… http… 2025-12-0… 2025-12-03T1…
#> 2 693074… 344e… docu… Un f… Un fichier… 2025-12-… http… 2025-12-0… 2025-12-03T1…
#> 3 693074… 0290… hist… title description 2025-12-… http… 2025-12-0… 2025-12-03T1…
#> 4 693074… 9884… docu… title description 2025-12-… http… 2025-12-0… 2025-12-03T1…
#> # ℹ 2 more variables: remote <lgl>, udataUrl <chr>

library(dplyr, warn.conflicts = FALSE)

# les pièces jointes d'un dataset en particulier
list_datasets() %>%
  filter(title == "Un jeu de données de test") %>%
  list_attachments()
#> # A tibble: 2 × 11
#>   id      rid   type  title description published url   created_at last_modified
#>   <chr>   <chr> <chr> <chr> <chr>       <chr>     <chr> <chr>      <chr>        
#> 1 693074… 4cea… docu… Un a… Un fichier… 2025-12-… http… 2025-12-0… 2025-12-03T1…
#> 2 693074… 344e… docu… Un f… Un fichier… 2025-12-… http… 2025-12-0… 2025-12-03T1…
#> # ℹ 2 more variables: remote <lgl>, udataUrl <chr>
```

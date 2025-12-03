# Liste les datafiles

Retourne la liste des datafiles ou si un objet dataset est passé en
argument, la liste des datafiles de ce dataset.

## Utilisation

``` r
list_datafiles(dataset = NULL)
```

## Arguments

- dataset:

  optionnel, un objet dataset

## Valeur de retour

un tibble avec les datafiles

## Voir également

Other datafile:
[`add_datafile()`](https://mtes-mct.github.io/didoscalim/reference/add_datafile.md),
[`add_or_update_datafile()`](https://mtes-mct.github.io/didoscalim/reference/add_or_update_datafile.md),
[`delete_datafile()`](https://mtes-mct.github.io/didoscalim/reference/delete_datafile.md),
[`dido_datafile()`](https://mtes-mct.github.io/didoscalim/reference/dido_datafile.md),
[`get_datafile()`](https://mtes-mct.github.io/didoscalim/reference/get_datafile.md),
[`update_datafile()`](https://mtes-mct.github.io/didoscalim/reference/update_datafile.md)

## Exemples

``` r
library(dplyr, warn.conflicts = FALSE)
# tous les datafiles
list_datafiles()
#> # A tibble: 4 × 13
#>   id     rid   title description created_at last_modified published legal_notice
#>   <chr>  <chr> <chr> <chr>       <chr>      <chr>         <chr>     <chr>       
#> 1 69307… f009… Un f… Un fichier… 2025-12-0… 2025-12-03T1… 2025-12-… SDES        
#> 2 69307… 4b1b… titre description 2025-12-0… 2025-12-03T1… 2025-12-… SDES        
#> 3 69307… 7215… titr… descriptio… 2025-12-0… 2025-12-03T1… 2025-12-… something   
#> 4 69307… 9e46… titre description 2025-12-0… 2025-12-03T1… 2025-12-… SDES        
#> # ℹ 5 more variables: url <chr>, millesimes <int>, millesimes_info <list>,
#> #   temporal_coverage.start <chr>, temporal_coverage.end <chr>

# les datafiles d'un jeu en particulier
list_datasets() %>%
  filter(title == "Un jeu de données de test") %>%
  list_datafiles()
#> # A tibble: 1 × 11
#>   id     rid   title description created_at last_modified published legal_notice
#>   <chr>  <chr> <chr> <chr>       <chr>      <chr>         <chr>     <chr>       
#> 1 69307… f009… Un f… Un fichier… 2025-12-0… 2025-12-03T1… 2025-12-… SDES        
#> # ℹ 3 more variables: url <chr>, millesimes <int>, millesimes_info <list>
```

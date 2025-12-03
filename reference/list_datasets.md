# Récupère la liste des datasets

Récupère la liste des datasets

## Utilisation

``` r
list_datasets()
```

## Valeur de retour

un tibble des datasets de l'utilisateur

## Exemples

``` r
list_datasets()
#> # A tibble: 10 × 21
#>    id         title description topic license frequency created_at last_modified
#>    <chr>      <chr> <chr>       <chr> <chr>   <chr>     <chr>      <chr>        
#>  1 693074aec… Un j… Un jeu de … Tran… fr-lo   unknown   2025-12-0… 2025-12-03T1…
#>  2 693074b43… Un d… Un dataset… Tran… fr-lo   unknown   2025-12-0… 2025-12-03T1…
#>  3 693074b4f… Donn… Données de… Tran… fr-lo   unknown   2025-12-0… 2025-12-03T1…
#>  4 693074b7c… Des … Descriptio… Tran… fr-lo   annual    2025-12-0… 2025-12-03T1…
#>  5 693074b73… Un d… Descriptio… Tran… fr-lo   unknown   2025-12-0… 2025-12-03T1…
#>  6 693074c2c… Donn… Données lo… Tran… fr-lo   unknown   2025-12-0… 2025-12-03T1…
#>  7 693074c2c… Donn… Données pa… Tran… ODbL-1… annual    2025-12-0… 2025-12-03T1…
#>  8 693074ce3… Un d… Descriptio… Tran… fr-lo   unknown   2025-12-0… 2025-12-03T1…
#>  9 693074cec… Un d… Descriptio… Tran… fr-lo   unknown   2025-12-0… 2025-12-03T1…
#> 10 693074dac… Un d… Descriptio… Tran… fr-lo   unknown   2025-12-0… 2025-12-03T1…
#> # ℹ 13 more variables: last_update <chr>, url <chr>, tags <list>,
#> #   attachments <list>, datafiles <list>, frequency_date <chr>, caution <chr>,
#> #   spatial.zones <list>, spatial.granularity <chr>, organization.id <chr>,
#> #   organization.title <chr>, temporal_coverage.start <chr>,
#> #   temporal_coverage.end <chr>
```

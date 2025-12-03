# Récupère les datasets devant être mis à jour

Récupère les datasets devant être mis à jour

## Utilisation

``` r
list_alerts()
```

## Valeur de retour

un tibble des datasets devant être mis à jour

## Exemples

``` r
list_alerts()
#> # A tibble: 2 × 21
#>   id         title description topic license frequency frequency_date created_at
#>   <chr>      <chr> <chr>       <chr> <chr>   <chr>     <chr>          <chr>     
#> 1 693074b7c… Des … Descriptio… Tran… fr-lo   annual    2022-01-10     2025-12-0…
#> 2 693074c2c… Donn… Données pa… Tran… ODbL-1… annual    2022-12-31     2025-12-0…
#> # ℹ 13 more variables: last_modified <chr>, last_update <chr>, url <chr>,
#> #   tags <list>, attachments <list>, datafiles <list>, caution <chr>,
#> #   spatial.zones <list>, spatial.granularity <chr>,
#> #   temporal_coverage.start <chr>, temporal_coverage.end <chr>,
#> #   organization.id <chr>, organization.title <chr>
```

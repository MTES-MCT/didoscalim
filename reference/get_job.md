# Récupére un job en particulier

Récupére un job en particulier

## Utilisation

``` r
get_job(data)
```

## Arguments

- data:

  l'id du job

## Valeur de retour

un objet
[`dido_job()`](https://mtes-mct.github.io/didoscalim/reference/dido_job.md)

## Exemples

``` r
job <- list_jobs()[1, ]
get_job(job)
#> List of 5
#>  $ id    : int 1
#>  $ owner : chr "693074a80c15d65cb39dc29f"
#>  $ state :List of 4
#>   ..$ status      : chr "complete"
#>   ..$ created_at  : chr "2025-12-03T17:34:39.118Z"
#>   ..$ started_at  : chr "2025-12-03T17:34:39.119Z"
#>   ..$ completed_at: chr "2025-12-03T17:34:39.617Z"
#>  $ data  :List of 6
#>   ..$ task                             : chr "createDatafile"
#>   ..$ file_name                        : chr "augmente.csv"
#>   ..$ dataset_id                       : chr "693074aecffe21e55a9ebb97"
#>   ..$ datafile_millesime               : chr "2021-12"
#>   ..$ datafile_metadata                :List of 4
#>   .. ..$ title       : chr "Un fichier de données de test 3"
#>   .. ..$ description : chr "Un fichier de données de test"
#>   .. ..$ published   : chr "2025-12-03T17:34:38.000Z"
#>   .. ..$ legal_notice: chr "SDES"
#>   ..$ datafile_millesime_date_diffusion: chr "2025-12-03T17:34:38.000Z"
#>  $ result:List of 6
#>   ..$ rid        : chr "f0095044-65d0-4db7-a1b9-d56509b76757"
#>   ..$ millesime  : chr "2021-12"
#>   ..$ duration   : int 1
#>   ..$ rows       : int 5
#>   ..$ csv_headers: int 8
#>   ..$ columns    : int 8
#>  - attr(*, "class")= chr [1:2] "dido_job" "list"
```

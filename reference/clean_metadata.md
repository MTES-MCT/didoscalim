# Prépare un objet pour l'envoyer à DiDo

Les objets
[`dido_dataset()`](https://mtes-mct.github.io/didoscalim/reference/dido_dataset.md)
et
[`dido_datafile()`](https://mtes-mct.github.io/didoscalim/reference/dido_datafile.md)
retournés par
[`get_dataset()`](https://mtes-mct.github.io/didoscalim/reference/get_dataset.md),
[`get_datafile()`](https://mtes-mct.github.io/didoscalim/reference/get_datafile.md),
... portent des informations sur les objets enfants (fichiers annexes,
datafiles, millésimes...). Cette fonction permet de les supprimer pour
ne garder que les métadonnées propres à l'objet.

## Utilisation

``` r
clean_metadata(data)
```

## Arguments

- data:

  objet dido_dataset, dido_datafile

## Valeur de retour

un objet du même type avec uniquement ses métadonnées propres.

## Exemples

``` r
dataset <- get_dataset(list_datasets()[1, ])
clean_metadata(dataset)
#> List of 13
#>  $ id           : chr "693074aecffe21e55a9ebb97"
#>  $ title        : chr "Un jeu de données de test"
#>  $ description  : chr "Un jeu de données de test"
#>  $ topic        : chr "Transports"
#>  $ license      : chr "fr-lo"
#>  $ frequency    : chr "unknown"
#>  $ spatial      :List of 2
#>   ..$ zones      : list()
#>   ..$ granularity: chr "other"
#>  $ created_at   : chr "2025-12-03T17:34:38.052Z"
#>  $ last_modified: chr "2025-12-03T17:35:05.424Z"
#>  $ last_update  : chr "2025-12-03T17:35:05.424Z"
#>  $ url          : chr "http://catalogue.diffusion.dido.fr/catalog.html?page=dataset&datasetId=693074aecffe21e55a9ebb97"
#>  $ tags         : list()
#>  $ organization :List of 2
#>   ..$ id   : chr "65f48cdcd67cf387d91bd392"
#>   ..$ title: chr "SDES"
#>  - attr(*, "class")= chr [1:2] "dido_dataset" "list"

datafile <- get_datafile(list_datafiles()[1, ])
clean_metadata(datafile)
#> List of 9
#>  $ rid          : chr "f0095044-65d0-4db7-a1b9-d56509b76757"
#>  $ title        : chr "Un fichier de données de test 3"
#>  $ description  : chr "Un fichier de données de test"
#>  $ created_at   : chr "2025-12-03T17:34:39.592Z"
#>  $ last_modified: chr "2025-12-03T17:35:05.422Z"
#>  $ published    : chr "2025-12-03T17:34:38.000Z"
#>  $ legal_notice : chr "SDES"
#>  $ url          : chr "http://catalogue.diffusion.dido.fr/catalog.html?page=datafile&datafileRid=f0095044-65d0-4db7-a1b9-d56509b76757"
#>  $ millesimes   : int 3
#>  - attr(*, "id")= chr "693074aecffe21e55a9ebb97"
#>  - attr(*, "class")= chr [1:2] "dido_datafile" "list"
```

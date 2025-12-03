# Envoie une requête au serveur DiDo

Envoie la requête au serveur et retourne le résultat sous forme d'un
objet ou un dataframe si as_dataframe est à TRUE

## Utilisation

``` r
dido_api(
  method,
  path,
  body = NULL,
  query_params = list(),
  headers = c(),
  as_tibble = FALSE,
  progress = FALSE,
  timeout_seconds = 900
)
```

## Arguments

- method:

  une des méthodes GET/POST/PUT/DELETE

- path:

  le chemin de l'api

- body:

  le body de la requête

- query_params:

  les paramètres de la requête

- headers:

  les entêtes de la requête

- as_tibble:

  TRUE/FALSE si TRUE retourne un tibble à la place d'un objet. Défaut à
  FALSE

- timeout_seconds:

  le timeout de la requête httr

## Valeur de retour

un objet json ou un dataframe

## Exemples

``` r
alerts <- dido_api(method = "GET", path = "/datasets/alerts", as_tibble = TRUE)
```

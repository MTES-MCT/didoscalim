# Attend la fin d'un job d'intégration

Attend la fin d'un job d'intégration

## Utilisation

``` r
wait_for_job(data)
```

## Arguments

- data:

  l'id du job

## Valeur de retour

les données du job

## Exemples

``` r
job <- list_jobs()[1, ] %>%
  wait_for_job()
```

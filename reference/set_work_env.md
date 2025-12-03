# Fixe l'environnement DiDo à utiliser

Fixe l'environnement DiDo à utiliser

## Utilisation

``` r
set_work_env(env_name = NULL, .envir = parent.frame())
```

## Arguments

- env_name:

  le nom de l'environnement à utiliser parmi PROD, ECOLE, PREPROD, DEV
  et suivant ce que vous avez configuré. Si env_name n'est pas passé,
  `set_didoscalim_work_env()` choisira le premier environnement
  configuré parmi DEV, PREPROD et ECOLE mais **jamais** PROD.

## Exemples

``` r
set_work_env("DEV")

set_work_env("PROD")

set_work_env()
```

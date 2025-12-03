# Recharge les environnements

Cette commande permet de recharger les environnements si vous avez
modifié le Renviron. Vous devez d'abord le relire avec
[`readRenviron()`](https://rdrr.io/r/base/readRenviron.html)

## Utilisation

``` r
load_envs()
```

## Exemples

``` r
readRenviron("~/.Renviron")
load_envs()
```

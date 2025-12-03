# Récupère l'environnement utilisé

Si aucun environnement n'a été fixé par
[`set_work_env()`](https://mtes-mct.github.io/didoscalim/reference/set_work_env.md),
`get_work_env()` retournera le premier environnement configuré en
suivant l'ordre : DEV, PREPROD, ECOLE. Il ne fixera **jamais** PROD
implicitement.

## Utilisation

``` r
get_work_env()
```

## Valeur de retour

une chaine de caractère avec l'environnement utilisé. Exemple "ECOLE"

## Exemples

``` r
get_work_env()
#> [1] "DEV"
```

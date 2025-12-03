# Vérifier l'accès aux différents environnements

Affiche un état des connexions aux différents environnements avec le
message d'erreur le cas échéant

## Utilisation

``` r
check_envs()
```

## Exemples

``` r
check_envs()
#> ✖ Environnement DiDo actif : DEV
#> ✖ Environnement DiDo actif : PREPROD
#> ✖ Environnement DiDo actif : PROD
#> Test de connexion:
#> ℹ DEV: OK
#> ✖ PREPROD: KO: ✖ Client error : 404 ℹ url: http://api.alimentation.dido.fr//users/me
#> ✖ PROD: KO: ✖ Client error : 404 ℹ url: http://api.alimentation.dido.fr/v1/users/me
#> ✖ Environnement DiDo actif : DEV
```

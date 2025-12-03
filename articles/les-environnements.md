# Configurer et utiliser les environnements

``` r
library(didoscalim)
#> ✖ Environnement DiDo actif : DEV
```

## Les environnements DiDo

Les données publiées sur l’environnement de production de DiDo (`PROD`)
sont immédiatement mis en ligne et disponibles sur internet.

Pour tester vos jeux de données, il est recommander d’utiliser un
environnement `ECOLE` en amont de la publication officielle. Cet
environnement permet de valider :

- le chargement proprement dit
- les métadonnées associées
- la qualité statistique

## Configuration

Les environnements se configurent dans le fichier `.Renviron`.

Chaque environnement est configuré par deux lignes comme celles-ci :

    DIDOSCALIM_BASE_PATH_<NOM_ENVIRONNEMENT>=http://api.alimentation.ecole.fr/v1
    DIDOSCALIM_API_KEY_<NOM_ENVIRONNEMENT>=<CLEF_API>

- `<NOM_ENVIRONNEMENT>` peut prendre les valeurs `DEV`, `PREPROD`,
  `ECOLE` et `PROD`
- `<CLEF_API>` est disponible dans l’interface d’alimentation DiDo dans
  la rubrique « Mon compte ».

Si vous êtes un producteur et que vous avez accès aux environnements
`ECOLE` et `PROD`, votre fichier `.Renviron` ressemblera à :

    DIDOSCALIM_BASE_PATH_ECOLE=http://api.alimentation.ecole.fr/v1
    DIDOSCALIM_API_KEY_ECOLE=ma_clef_api_ecole

    DIDOSCALIM_BASE_PATH_PROD=http://api.alimentation.prod.fr/v1
    DIDOSCALIM_API_KEY_PROD=ma_clef_api_prod

Pour le modifier vous pouvez utiliser les commandes suivantes :

``` r
library(devtools)
usethis::edit_r_environ()
readRenviron("~/.Renviron")
library(didoscalim)
load_envs()
check_envs()
```

## Choisir un environnement

Si vous ne sélectionnez pas d’environnement, DiDo prendra
automatiquement par défaut le premier configuré dans l’ordre : `DEV`,
`PREPROD`, `ECOLE`. Il ne choisira **jamais** `PROD` implicitement.

Pour sélectionner un environnement, utilisez la commande
[`set_work_env()`](https://mtes-mct.github.io/didoscalim/reference/set_work_env.md)
:

``` r
set_work_env("PROD")
#> ✖ Environnement DiDo actif : PROD
```

Pour revenir à l’environnement par défaut :

``` r
set_work_env()
#> ✖ Environnement DiDo actif : DEV
```

Vous pouvez à tout moment voir dans quel environnement vous êtes avec la
commande
[`get_work_env()`](https://mtes-mct.github.io/didoscalim/reference/get_work_env.md).

Vous pouvez lister les environnements configués avec la commande
[`list_envs()`](https://mtes-mct.github.io/didoscalim/reference/list_envs.md)
:

``` r
list_envs()
#> Vous avez 3 environnement(s) configuré(s) : 
#> ℹ DEV : http://alim.dido.fr/api/v1
#> ℹ PREPROD : http://api.alimentation.dido.fr/
#> ℹ PROD : http://api.alimentation.dido.fr/v1
```

et vérifier qu’ils fonctionnent avec la commande
[`check_envs()`](https://mtes-mct.github.io/didoscalim/reference/check_envs.md)
:

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

## Méthode d’alimentation

Écrivez et testez vos scripts dans l’environnement `ECOLE` par défaut.
Quand vous êtes satisfait du résultat, basculer sur l’environnement de
`PROD` et relancez les scripts.

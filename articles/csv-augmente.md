# Générer un fichier csv augmenté

``` r
library(didoscalim)
#> ✖ Environnement DiDo actif : DEV
```

## Le CSV augmenté

DiDo utilise un csv augmenté avec 4 lignes d’entêtes :

1.  une description des variables
2.  le type de la variable
3.  l’unité de la variable
4.  le nom de la variable

&nbsp;

    "Commune";"Nombre de logements neufs"
    "cog_commune_2020";"entier"
    "n/a";"s/u"
    "COMMUNE";"LOGEMENTS_NEUFS"

Le [site de documentation de
l’API](https://cgdd.gitlab-pages.din.developpement-durable.gouv.fr/sdsed-bun/dido/api/)
propose une documentation plus complète sur [le csv
augmenté](https://cgdd.gitlab-pages.din.developpement-durable.gouv.fr/sdsed-bun/dido/api/040-csvfile/)
ainsi que sur [la liste des
types](https://cgdd.gitlab-pages.din.developpement-durable.gouv.fr/sdsed-bun/dido/api/210-headers/).

## Générer un CSV augmenté.

`didoscalim` propose une fonction pour vous aider.

La génération se passe en trois étapes :

1.  le chargement du fichier d’origine :
    [`dido_read_delim()`](https://mtes-mct.github.io/didoscalim/reference/dido_read_delim.md)
2.  l’ajout des lignes d’entêtes spécifiques :
    [`dido_csv()`](https://mtes-mct.github.io/didoscalim/reference/dido_csv.md)
3.  l’écriture du fichier au bon format :
    [`dido_write_csv()`](https://mtes-mct.github.io/didoscalim/reference/dido_write_csv.md)

## le chargement du fichier d’origine

Le fichier d’origine doit être au format CSV classique, ie, la première
ligne du fichier contient le nom des variables. Vous pouvez télécharger
le [fichier exemple
utilisé](https://mtes-mct.github.io/didoscalim/articles/exemple.csv).

En premier lieu, charger le fichier dans un dataframe avec la commande
[`dido_read_delim()`](https://mtes-mct.github.io/didoscalim/reference/dido_read_delim.md).

``` r
data <- dido_read_delim("exemple.csv")
knitr::kable(head(data, n = 5))
```

| OPE   | ANNEE | FILIERE | EPCI      | CAT     | NAF2 | GS  | CONSO  |
|:------|:------|:--------|:----------|:--------|:-----|:----|:-------|
| Aé    | 2019  | G       | 200036465 | RES     | NA   | R   | 1039   |
| ESL   | 2019  | G       | 200034023 | ENT_PRO | NA   | I   | 552    |
| ÇÀ    | 2019  | G       | 200034270 | RES     | NA   | R   | 75914  |
| RGDS  | 2019  | G       | 200034635 | ENT     | 10   | I   | 454    |
| SîCÂE | 2019  | G       | 200037059 | ENT_PRO | NA   | I   | 288821 |

Si votre fichier est dans un format autre que `UTF-8` ou que le
séparateur de champ est la virgule (`,`) à vous devez préciser `delim`
et/ou `locale` :

``` r
dido_read_delim(dido_example("csv-win-char.csv"),
                delim = ",",
                locale = locale(encoding = "WINDOWS-1252")
)
#> # A tibble: 5 × 8
#>   OPE   ANNEE FILIERE EPCI      CAT     NAF2  GS    CONSO 
#>   <chr> <chr> <chr>   <chr>     <chr>   <chr> <chr> <chr> 
#> 1 Aé    2019  G       200036465 RES     NA    R     1039  
#> 2 ESL   2019  G       200034023 ENT_PRO NA    I     552   
#> 3 ÇÀ    2019  G       200034270 RES     NA    R     75914 
#> 4 RGDS  2019  G       200034635 ENT     10    I     454   
#> 5 SîCÂE 2019  G       200037059 ENT_PRO NA    I     288821
```

## L’ajout des lignes d’entêtes spécifiques

didoscalim peut analyser le fichier et proposer un premier niveau de
description.

``` r
result <- dido_csv(data)
knitr::kable(head(result, n = 5))
```

| OPE   | ANNEE                 | FILIERE | EPCI           | CAT   | NAF2   | GS    | CONSO  |
|:------|:----------------------|:--------|:---------------|:------|:-------|:------|:-------|
| OPE   | Millésime des données | FILIERE | Code de l’EPCI | CAT   | NAF2   | GS    | CONSO  |
| texte | annee                 | texte   | cog_epci_2025  | texte | entier | texte | entier |
| n/a   | n/a                   | n/a     | n/a            | n/a   | s/u    | n/a   | s/u    |
| OPE   | ANNEE                 | FILIERE | EPCI           | CAT   | NAF2   | GS    | CONSO  |
| Aé    | 2019                  | G       | 200036465      | RES   | NA     | R     | 1039   |

`didoscalim` reconnait les champs `EPCI` et `ANNEE` et propose des
descriptions/types/unités raisonnables. Il est recommandé de les garder.
Le millésime des champs `cog_*` comme celui du champ `EPCI` est par
défaut l’année en cours, vous pouvez préciser le millésime à utiliser
avec le paramètre `cog_year` (cf exemple complet ci-dessous).

Si le fichier source utilise la virgule (`,`) comme séparateur décimal,
vous devez préciser l’argument `locale` :

``` r
temp <- dido_csv(
  data,
  locale = locale(decimal_mark = ',')
)
```

Pour aller plus loin, vous pouvez soit écrire le fichier tel quel avec
[`dido_write_csv()`](https://mtes-mct.github.io/didoscalim/reference/dido_write_csv.md)
et l’éditer à la main. Soit utiliser le paramètre `params` de la
commande
[`dido_csv()`](https://mtes-mct.github.io/didoscalim/reference/dido_csv.md).

Voici un exemple complet pour le fichier exemple :

``` r
params = list(
  OPE = list(name = "OPERATEUR", description = "Nom de l'opérateur"),
  FILIERE = list(description = "Filière"),
  CAT = list(description = "Catégorie de la consommation"),
  NAF2 = list(description = "Code NAF à 2 positions du secteur (NAF rev2 2008)", type = "naf_division"),
  CONSO = list(description = "Consommation (en MWh)", unit = "MWh")
)
result <- dido_csv(data, params = params, cog_year = 2020)
knitr::kable(head(result, n = 5))
```

| OPE | ANNEE | FILIERE | EPCI | CAT | NAF2 | GS | CONSO |
|:---|:---|:---|:---|:---|:---|:---|:---|
| Nom de l’opérateur | Millésime des données | Filière | Code de l’EPCI | Catégorie de la consommation | Code NAF à 2 positions du secteur (NAF rev2 2008) | GS | Consommation (en MWh) |
| texte | annee | texte | cog_epci_2020 | texte | naf_division | texte | entier |
| n/a | n/a | n/a | n/a | n/a | n/a | n/a | MWh |
| OPERATEUR | ANNEE | FILIERE | EPCI | CAT | NAF2 | GS | CONSO |
| Aé | 2019 | G | 200036465 | RES | NA | R | 1039 |

## travailler avec un fichier source volumineux

Si vous travaillez sur un fichier de données volumineux, la méthode
`dido_csv` peut prendre un temps de traitement certain.

Pour tester la génération des entêtes du CSV augmenté, vous pouvez
travailler sur un sous-ensemble de votre dataframe :

``` r
result <- data %>% head(10000) %>% dido_csv(params = params, cog_year = 2020)
```

Attention, toutefois, si vous vous appuyez sur la détection automatique
des colonnes, utilisez un nombre de lignes suffisamment grand : 10 000
voire 1 000 lignes devrait suffire dans la majeure partie des cas pour
réduire le temps de traitement largement en dessous de la seconde tout
en permettant une bonne détection automatique.

## L’écriture du fichier au bon format.

``` r
dido_write_csv(result, "resultat.csv")
```

Vous pouvez télécharger [le fichier
généré](https://mtes-mct.github.io/didoscalim/articles/resultat.csv)

## résumé.

Une fois que la configuration de `params` est correcte, vous pouvez
chainer avec l’opérateur `magrittr::%>%` :

``` r
require(magrittr)
#> Le chargement a nécessité le package : magrittr
dido_read_delim(dido_example("exemple.csv")) %>%
  dido_csv(params = params) %>%
  dido_write_csv("/tmp/resultat.csv")
```

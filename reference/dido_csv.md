# Génère les lignes d'entête du CSV augmenté

Génère un dataframe avec les lignes d'entêtes du CSV augmenté comme
premières lignes.

## Utilisation

``` r
dido_csv(
  data,
  params = list(),
  locale = readr::default_locale(),
  cog_year = format(Sys.time(), "%Y")
)
```

## Arguments

- data:

  le dataframe à augmenter

- params:

  une liste nommée décrivant les caractéristiques des colonnes :

      list(
        COL1 = list(description = "une description"),
        COL2 = list(unit = "MWh")
      )

  Les caractéristiques disponibles sont :

  - `name`: le nom de la colonne

  - `description`: la description de la colonne

  - `type`: nombre, entier, texte, ...

  - `unit`: l'unité de la colonne

  Le nom de la colonne peut être une expression rationnelle :

      list(
        COL     = list(description = "une description"),
        `COL.*` = list(unit = "MWh")
      )

  La première colonne correspondante est utilisée, **mettez toujours vos
  expressions rationnelles à la fin**.

  La valeur du champ `description` est évaluée par glue::glue avec un
  paramètre `name` qui correspond au nom de la variable du dataframe
  d'origine.

      list(
        COL     = list(description = "une description"),
        `COL.*` = list(
          unit = "MWh",
          description = "une description {string::str_extract(name, '\\d{4}')}"
        )
      )

- locale:

  la locale à utiliser. Le seul élément à configurer à ce niveau est le
  séparateur décimal. Par défaut c'est le point (`.`). Pour sélectionner
  la virgule vous pouvez utiliser :

      locale = locale(decimal_mark = ",")

- cog_year:

  le millésime du COG utilisé si besoin. Par défaut prend l'année en
  cours

## Valeur de retour

un dataframe avec les 4 lignes de description du csv augmenté

## Détails

Cette fonction essaye de deviner le type des colonnes pour leur donner
un type DiDo de base :

|                 |                |
|-----------------|----------------|
| type R          | type en sortie |
| double          | nombre         |
| integer         | entier         |
| date            | jour           |
| logical         | booleen        |
| tous les autres | texte          |

\*\* il est fortement déconseillé d'utiliser le type DiDo `nombre`.
Utilisez plutôt un nombre avec précision comme par exemple
`nombre(2)`\*\*

Certains noms de variable sont connus par didoscalim qui génère
automatiquement le type et la description. La liste complète de ces
variables et des types/descriptions associés est :

|                    |                      |                        |
|--------------------|----------------------|------------------------|
| nom de la variable | type                 | description            |
| REGION             | cog_region_AAAA      | Code de la région      |
| DEPARTEMENT        | cog_departement_AAAA | Code du département    |
| COMMUNE            | cog_commune_AAAA     | Code de la commune     |
| EPCI               | cog_epci_AAAA        | Code de l'EPCI         |
| IRIS               | cog_iris_AAAA        | Code de l'IRIS         |
| ANNEE              | n/a                  | Millésimes des données |
| MOIS               | n/a                  | mois des données       |

L'année `AAAA` est par défaut l'année courante, vous pouvez la modifier
en passant le paramètre `cog_year`

## Voir également

En complément, vous pouvez lire : [la vignette dédiée à la génération
des fichiers
augmentés](https://mtes-mct.github.io/didoscalim/articles/csv-augmente.html)

Et la documentation de l'API :

- [la description d'un fichier csv
  augmenté](https://cgdd.gitlab-pages.din.developpement-durable.gouv.fr/sdsed-bun/datalake/api/040-csvfile/),

- [la liste des entêtes
  utilisables](https://cgdd.gitlab-pages.din.developpement-durable.gouv.fr/sdsed-bun/datalake/api/210-headers/)

Other csv:
[`dido_read_delim()`](https://mtes-mct.github.io/didoscalim/reference/dido_read_delim.md),
[`dido_write_csv()`](https://mtes-mct.github.io/didoscalim/reference/dido_write_csv.md)

## Exemples

``` r
data <- data.frame(
  OPERATEUR = c("nom1", "nom2"),
  COMMUNE = c("29000", "35000"),
  CONSO = c(1, 2)
)
params <- list(
  OPERATEUR = list(description = "L'opérateur"),
  CONSO = list(name = "CONSOMMATION", description = "La consommation", unit = "Mwh")
)
dido_csv(data, params = params)
#> # A tibble: 6 × 3
#>   OPERATEUR   COMMUNE            CONSO          
#>   <chr>       <chr>              <chr>          
#> 1 L'opérateur Code de la commune La consommation
#> 2 texte       cog_commune_2025   entier         
#> 3 n/a         n/a                Mwh            
#> 4 OPERATEUR   COMMUNE            CONSOMMATION   
#> 5 nom1        29000              1              
#> 6 nom2        35000              2              

data <- data.frame(
  DONNEES_2021 = c("1,4", "1,5"),
  DONNEES_2022 = c("1,3", "1,8")
)
params <- list(
  `DONNEES_.*` = list(
    description = 'description pour {stringr::str_extract(name, "\\\\d{4}")}'
  )
)
dido_csv(data, params = params, locale = locale(decimal_mark = ","))
#> Warning: ✖ Vous utilisez un type `nombre` dans vos entêtes.
#> ℹ Il est recommandé d'utiliser un type nombre avec précision : `nombre(2)`.
#> # A tibble: 6 × 2
#>   DONNEES_2021          DONNEES_2022         
#>   <chr>                 <chr>                
#> 1 description pour 2021 description pour 2022
#> 2 nombre                nombre               
#> 3 s/u                   s/u                  
#> 4 DONNEES_2021          DONNEES_2022         
#> 5 1,4                   1,3                  
#> 6 1,5                   1,8                  
```

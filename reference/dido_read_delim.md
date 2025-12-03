# Lit un fichier CSV

Cette fonction utilise directement
[`readr::read_delim`](https://readr.tidyverse.org/reference/read_delim.html)
en enlevant la détection du type des colonnes et en fixant `delim` à ";"
par défaut.

## Utilisation

``` r
dido_read_delim(file, delim = ";", ...)
```

## Arguments

- file:

  Either a path to a file, a connection, or literal data (either a
  single string or a raw vector).

  Files ending in `.gz`, `.bz2`, `.xz`, or `.zip` will be automatically
  uncompressed. Files starting with `http://`, `https://`, `ftp://`, or
  `ftps://` will be automatically downloaded. Remote gz files can also
  be automatically downloaded and decompressed.

  Literal data is most useful for examples and tests. To be recognised
  as literal data, the input must be either wrapped with
  [`I()`](https://rdrr.io/r/base/AsIs.html), be a string containing at
  least one new line, or be a vector containing at least one string with
  a new line.

  Using a value of
  [`clipboard()`](https://readr.tidyverse.org/reference/clipboard.html)
  will read from the system clipboard.

- delim:

  le séparateur de champ. Par défaut ";".

- ...:

  ces arguments seront passés directement à la fonction
  [`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html)

- locale:

  la locale à utiliser pour lire les fichiers. A ce niveau, le paramètre
  le plus important est l'encodage du fichier. Le défaut est "UTF-8".
  Les autres encodages fréquemment rencontrés sont "WINDOWS-1252" et
  "ISO-8859-15"

## Valeur de retour

un tibble dont toutes les colonnes sont de type `chr`

## Détails

Certaines variables peuvent avoir des valeurs secrétisées représentées
par la valeur `secret`, la détection automatique du package `readr`
n'est donc pas fiable et est désactivé à ce niveau. La détection
automatique est faite dans la fonction
[`dido_csv()`](https://mtes-mct.github.io/didoscalim/reference/dido_csv.md).

## Voir également

Other csv:
[`dido_csv()`](https://mtes-mct.github.io/didoscalim/reference/dido_csv.md),
[`dido_write_csv()`](https://mtes-mct.github.io/didoscalim/reference/dido_write_csv.md)

## Exemples

``` r
dido_read_delim(dido_example("exemple.csv"))
#> # A tibble: 5 × 8
#>   OPE   ANNEE FILIERE EPCI      CAT     NAF2  GS    CONSO 
#>   <chr> <chr> <chr>   <chr>     <chr>   <chr> <chr> <chr> 
#> 1 Aé    2019  G       200036465 RES     NA    R     1039  
#> 2 ESL   2019  G       200034023 ENT_PRO NA    I     552   
#> 3 ÇÀ    2019  G       200034270 RES     NA    R     75914 
#> 4 RGDS  2019  G       200034635 ENT     10    I     454   
#> 5 SîCÂE 2019  G       200037059 ENT_PRO NA    I     288821
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

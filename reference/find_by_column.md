# cherche dans un tibble

cherche dans un tibble

## Utilisation

``` r
find_by_column(data, string, col, return = c("id"))
```

## Arguments

- data:

  le dataframe/tibble dans lequel rechercher

- string:

  la chaine à chercher

- col:

  la colonne dans laquelle chercher

- return_col:

  a vecteur de colonnes à retourner

## Valeur de retour

un id

## Exemples

``` r
if (FALSE) { # \dontrun{
find_by_column(data, "un titre", "title")
find_by_column(data = data, string = "un titre", "title", c("id", "rid"))
find_by_column(data = data, string = stringr::fixed("rid"), "rid")
} # }
```

# Enregistre le fichier CSV augmenté généré

Enregistre le fichier CSV augmenté généré

## Utilisation

``` r
dido_write_csv(data, file)
```

## Arguments

- data:

  un tibble retourné par
  [`dido_csv()`](https://mtes-mct.github.io/didoscalim/reference/dido_csv.md)

- file:

  le nom du fichier

## Valeur de retour

le tibble passé en entrée

## Voir également

Other csv:
[`dido_csv()`](https://mtes-mct.github.io/didoscalim/reference/dido_csv.md),
[`dido_read_delim()`](https://mtes-mct.github.io/didoscalim/reference/dido_read_delim.md)

## Exemples

``` r
dido_read_delim(dido_example("exemple.csv")) %>%
  dido_csv() %>%
  dido_write_csv("/tmp/fichier.csv")
```

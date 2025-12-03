# Remplacer les données d'un millésime

Cette méthode remplace les données d'un millésime en conservant son
identifiant.

## Utilisation

``` r
replace_millesime(
  datafile,
  file_name,
  millesime,
  date_diffusion = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)
```

## Arguments

- datafile:

  un objet dido_datafile obtenu par
  [`get_datafile()`](https://mtes-mct.github.io/didoscalim/reference/get_datafile.md)

- file_name:

  le nom du fichier à charger

- millesime:

  le millesime (AAAA-MM). Par défaut AAAA-MM avec l'année courante et le
  mois courant

- date_diffusion:

  (optionnel) les date et heure auxquelles le fichier devra être rendu
  accessible à la diffusion.

  Ce paramètre est au format "AAAA-MM-JJ HH:MM:SS" (ou ISO8601 si vous
  préférez). Si la timezone n'est pas précisée, la timezone de
  l'ordinateur local est utilisée.

  Si cette date/heure est dans le passé, les données sont immédiatement
  accessibles, si elle est dans le futur, les données ne seront
  accessibles qu'à cette date/heure.

  Si ce paramètre est non précisé prend la date/heure courante, les
  données sont donc immédiatement accessibles.

## Valeur de retour

un objet
[`dido_job()`](https://mtes-mct.github.io/didoscalim/reference/dido_job.md)

## Voir également

Other millesime:
[`add_millesime()`](https://mtes-mct.github.io/didoscalim/reference/add_millesime.md),
[`delete_millesime()`](https://mtes-mct.github.io/didoscalim/reference/delete_millesime.md),
[`list_millesimes()`](https://mtes-mct.github.io/didoscalim/reference/list_millesimes.md)

## Exemples

``` r
library(dplyr, warn.conflicts = FALSE)

datafile <- list_datafiles() %>%
  filter(title == "Un fichier de données de test")

millesime <- replace_millesime(
  datafile = datafile,
  file_name = dido_example("augmente.csv"),
  millesime = "2021-12"
)
#> Error in abort_if_not_one_line("data"): ✖ L'argument `data` doit contenir une ligne.
#> ℹ `data` contient 0 ligne(s).
#> ℹ Avez-vous oublié de filtrer (avec dplyr::filter par exemple`) le dataframe avant de le passer en
#> argument ?
```

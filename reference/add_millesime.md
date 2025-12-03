# Ajoute un millésime à un datafile

Ajoute un millésime à un datafile

## Utilisation

``` r
add_millesime(
  datafile,
  file_name,
  date_diffusion = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  millesime = format(Sys.time(), "%Y-%m")
)
```

## Arguments

- datafile:

  un objet dido_datafile obtenu par
  [`get_datafile()`](https://mtes-mct.github.io/didoscalim/reference/get_datafile.md)

- file_name:

  le nom du fichier à charger

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

- millesime:

  le millesime (AAAA-MM). Par défaut AAAA-MM avec l'année courante et le
  mois courant

## Valeur de retour

un objet
[`dido_job()`](https://mtes-mct.github.io/didoscalim/reference/dido_job.md)

## Voir également

Other millesime:
[`delete_millesime()`](https://mtes-mct.github.io/didoscalim/reference/delete_millesime.md),
[`list_millesimes()`](https://mtes-mct.github.io/didoscalim/reference/list_millesimes.md),
[`replace_millesime()`](https://mtes-mct.github.io/didoscalim/reference/replace_millesime.md)

## Exemples

``` r
library(dplyr, warn.conflicts = FALSE)

datafile <- list_datafiles() %>%
  slice(1) %>%
  get_datafile()

millesime <- add_millesime(
  datafile = datafile,
  file_name = dido_example("augmente.csv"),
  millesime = "2011-10"
)
#>     intégration du fichier `/home/nc/travail/R/didoscalim/inst/extdata/augmente.csv`
#>  * fichier versé
#>  * fichier intégré:   rid: f0095044-65d0-4db7-a1b9-d56509b76757   millesime: 2011-10  lignes: 5

# publier un millésime avec embargo
millesime <- add_millesime(
  datafile = datafile,
  file_name = dido_example("augmente.csv"),
  millesime = "2012-10",
  date_diffusion = "2011-10-10 08:00:00"
)
#>     intégration du fichier `/home/nc/travail/R/didoscalim/inst/extdata/augmente.csv`
#>  * fichier versé
#>  * fichier intégré:   rid: f0095044-65d0-4db7-a1b9-d56509b76757   millesime: 2012-10  lignes: 5
```

# Ajoute ou modifie un datafile

met à jour le datafile (métadonnées et données) avec le même titre s'il
existe sinon ajoute un datafile, un millésime ou remplace un millésime
s'il existe déjà.

Le paramètre `on_existing_millesime` permet de préciser le comportement
à adopter si le millésime existe déjà.

Si plusieurs datafiles ont le même titre, cette méthode retourne une
erreur.

Si vous devez mettre à jour des datafiles, il est conseillé d'utiliser
la fonction
[`didoscalim_update_only()`](https://mtes-mct.github.io/didoscalim/reference/didoscalim_update_only.md),
cf
[`vignette("complements-sur-les-mises-a-jour")`](https://mtes-mct.github.io/didoscalim/articles/complements-sur-les-mises-a-jour.md)

## Utilisation

``` r
add_or_update_datafile(
  dataset,
  title,
  description,
  file_name,
  millesime = format(Sys.time(), "%Y-%m"),
  published = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  temporal_coverage_start = NULL,
  temporal_coverage_end = NULL,
  legal_notice = "SDES",
  date_diffusion = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  keep_old_millesimes = Inf,
  on_existing_millesime = "fail",
  check_file_date = FALSE
)
```

## Arguments

- dataset:

  l'id d'un dataset, un objet
  [`dido_dataset()`](https://mtes-mct.github.io/didoscalim/reference/dido_dataset.md),
  [`dido_datafile()`](https://mtes-mct.github.io/didoscalim/reference/dido_datafile.md)
  ou
  [`dido_job()`](https://mtes-mct.github.io/didoscalim/reference/dido_job.md)

- title:

  le titre du datafile

- description:

  la description du datafile

- file_name:

  le nom du fichier à charger

- millesime:

  le millesime (AAAA-MM). Par défaut AAAA-MM avec l'année courante et le
  mois courant

- published:

  la date/heure de publication du fichier, si non précisée, prend la
  date/heure du moment.

  Ce paramètre est au format "AAAA-MM-JJ HH:MM:SS" (ou ISO8601 si vous
  préférez). Si la timezone n'est pas précisée, la timezone de
  l'ordinateur local est utilisée.

- temporal_coverage_start:

  optionnel, la date de début de couverture du fichier de données au
  format AAAA-MM-JJ

- temporal_coverage_end:

  optionnel, la date de fin de couverture du fichier de données au
  format AAAA-MM-JJ

- legal_notice:

  les mentions légales, par défaut "SDES"

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

- keep_old_millesimes:

  nombre d'ancien millésimes à conserver. Ce paramètre permet de
  supprimer les anciens millésimes. Pour des raisons de sécurité, cette
  option n'effacera jamais des millésimes plus récent que le millésime
  passé en paramètre.

- on_existing_millesime:

  skip/fail/replace : action à faire quand le millésime existe déjà.
  Peut-être :

  - "skip" : retourne immédiatement après avoir affiché un message. Les
    métadonnées du millésime ne sont pas mises à jour et les anciens
    millésimes ne sont jamais effacés.

  - "fail" : lève une exception de class `existing_millesime`. Les
    métadonnées du millésime ne sont pas mises à jour et les anciens
    millésimes ne sont jamais effacés.

  - "replace" : remplace le millésime ayant le même identifiant

- check_file_date:

  TRUE/FALSE, Si TRUE met à jour le datafile uniquement si le fichier
  est plus récent que le last_modified du datafile

## Valeur de retour

un objet
[`dido_job()`](https://mtes-mct.github.io/didoscalim/reference/dido_job.md)
ou `NULL` si aucune création/mise à jour n'a eu lieu

## Voir également

Other datafile:
[`add_datafile()`](https://mtes-mct.github.io/didoscalim/reference/add_datafile.md),
[`delete_datafile()`](https://mtes-mct.github.io/didoscalim/reference/delete_datafile.md),
[`dido_datafile()`](https://mtes-mct.github.io/didoscalim/reference/dido_datafile.md),
[`get_datafile()`](https://mtes-mct.github.io/didoscalim/reference/get_datafile.md),
[`list_datafiles()`](https://mtes-mct.github.io/didoscalim/reference/list_datafiles.md),
[`update_datafile()`](https://mtes-mct.github.io/didoscalim/reference/update_datafile.md)

## Exemples

``` r
library(dplyr, warn.conflicts = FALSE)

dataset <- add_or_update_dataset(
  title = "Un dataset pour les add_or_update_datafiles",
  description = "Description des données statistiques",
  topic = "Transports",
  frequency = "unknown",
)
#> dataset `Un dataset pour les add_or_update_datafiles` créé

add_or_update_datafile(
  dataset = dataset,
  title = "titre",
  description = "description",
  file_name = dido_example("augmente.csv"),
  millesime = "2021-10",
)
#>     intégration du fichier `/home/nc/travail/R/didoscalim/inst/extdata/augmente.csv`
#>  * fichier versé
#>  * fichier intégré:   rid: 9e463695-c0bc-4317-9ad3-52894038c6c8   millesime: 2021-10  lignes: 5

add_or_update_datafile(
  dataset = dataset,
  title = "titre",
  description = "description",
  file_name = dido_example("augmente.csv"),
  temporal_coverage_start = "2021-01-01",
  temporal_coverage_end = "2021-12-31",
  millesime = "2022-10",
)
#>     intégration du fichier `/home/nc/travail/R/didoscalim/inst/extdata/augmente.csv`
#>  * fichier versé
#>  * fichier intégré:   rid: 9e463695-c0bc-4317-9ad3-52894038c6c8   millesime: 2022-10  lignes: 5
#> datafile "titre" modifié
```

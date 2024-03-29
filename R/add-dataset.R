#' Ajoute un dataset
#'
#' @param title le titre du dataset
#' @param description la description du dataset
#' @param topic le thème du dataset. Doit être dans "Environnement", "Énergie",
#'   "Transports", "Logement", "Changement climatique"
#' @param tags la liste des mots clefs. Si le fichier est déjà publié sur le
#'   site SDES, on reprend les mêmes. La liste des [mots clefs recensés](https://data.statistiques.developpement-durable.gouv.fr/dido/api/v1/referentiels/tags/csv?withColumnName=true&withColumnDescription=false).
#' @param frequency la fréquence de publication des données. Les fréquences les
#'   plus utilisées sont `annual`, `monthly`, `ponctual`, `unknown`. [la liste
#'   complète des
#'   fréquences](https://data.statistiques.developpement-durable.gouv.fr/dido/api/v1/metadata/frequencies)
#'
#'
#' @param frequency_date la date de prochaine publication. optionnel seulement
#'   la `frequency` est `unknown`.
#' @param organization l'id de l'organisation sous laquelle vous souhaitez
#'   publier le dataset, si vous n'appartenez qu'à une seule organisation, vous
#'   n'avez pas à remplir ce champ, didoscalim la prendra par défaut.
#'
#'   Si ça n'est pas le cas, vous devez préciser la fonction `my_organization()`
#' @param temporal_coverage_start optionnel, la date de début de couverture du
#'   jeux de données au format AAAA-MM-JJ
#' @param temporal_coverage_end  optionnel, la date de fin de couverture du jeux
#'   de données au format AAAA-MM-JJ
#' @param zones la zone couverte par le jeu de données. `country:fr` pour France
#'   entière, `country-subset:fr:metro` pour la métropole et
#'   `country-subset:fr:drom`` pour les DROM
#' @param granularity la granularité du jeu de données. Les plus utilisés sont
#'   "fr:region", "fr:departement", "fr:epci", "fr:commune", "fr:iris" et pour
#'   les données à l'adresse "poi". [La liste complète des
#'   granularités](https://data.statistiques.developpement-durable.gouv.fr/dido/api/v1/metadata/granularities)
#'
#' @param caution Les précautions à prendre avec ce jeu de données. Par exemple
#'   : "certaines données peuvent être secrétisées"
#' @param license la licence des données. "fr-lo" par défaut
#'
#' @return un objet [dido_dataset()]. Ce dernier sert pour créer par la suite
#'   les datafiles et les millésimes.
#'
#' @family dataset
#'
#' @export
#'
#' @seealso La liste des [mots clefs recensés](https://data.statistiques.developpement-durable.gouv.fr/dido/api/v1/referentiels/tags/csv?withColumnName=true&withColumnDescription=false)
#'
#' @examples
#' dataset <- add_dataset(
#'   title = "Données logements",
#'   description = "Données logement en date réelle",
#'   topic = "Transports",
#'   frequency = "unknown"
#' )
#'
#' dataset <- add_dataset(
#'   title = "Données parc de véhicule",
#'   description = "Données parc de véhicule",
#'   topic = "Transports",
#'   tags = list("transport", "vehicule", "circulation"),
#'   frequency = "annual",
#'   organization = my_organization(),
#'   frequency_date = "2022-12-31",
#'   temporal_coverage_start = "2021-01-01",
#'   temporal_coverage_end = "2021-12-31",
#'   granularity = "fr:region",
#'   zones = "country:fr",
#'   caution = "caution",
#'   license = "ODbL-1.0"
#' )
add_dataset <- function(title,
                        description,
                        topic,
                        frequency,
                        tags = NULL,
                        frequency_date = NULL,
                        granularity = NULL,
                        zones = NULL,
                        organization = my_organization(),
                        license = "fr-lo",
                        temporal_coverage_start = NULL,
                        temporal_coverage_end = NULL,
                        caution = NULL) {
  dataset <- dido_dataset(
    title = title,
    description = description,
    topic = topic,
    frequency = frequency,
    tags = tags,
    frequency_date = frequency_date,
    granularity = granularity,
    zones = zones,
    organization = organization,
    license = license,
    temporal_coverage_start = temporal_coverage_start,
    temporal_coverage_end = temporal_coverage_end,
    caution = caution
  )

  body <- jsonlite::toJSON(dataset, pretty = TRUE, auto_unbox = TRUE, na = "null")
  ds <- dido_api(method = "POST", path = "/datasets", body = body)

  didoscalim_info(glue::glue("dataset `{title}` créé"))
  get_dataset(ds$id)
}

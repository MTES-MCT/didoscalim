#' Ajoute ou met à jour un dataset
#'
#' Cette fonction permet, par exemple, de modifier la période de couverture du
#' dataset ainsi que la date de prochaine mise à jour.
#'
#' @inheritParams add_dataset
#'
#' @return un objet [dido_dataset()]. Ce dernier sert pour créer par la suite
#'   les datafiles et les millésimes.
#'
#' @family dataset
#'
#' @export
#'
#' @seealso La liste des [tags/mots clefs recensés](https://data.statistiques.developpement-durable.gouv.fr/dido/api/v1/referentiels/tags/csv?withColumnName=true&withColumnDescription=false)
#'
#' @examples
#' dataset <- add_or_update_dataset(
#'   title = "Des données statistiques",
#'   description = "Description des données statistiques",
#'   topic = "Transports",
#'   frequency = "annual",
#'   frequency_date = "2021-01-10",
#'   temporal_coverage_start = "2020-01-01",
#'   temporal_coverage_end = "2020-12-31"
#' )
#'
#' dataset <- add_or_update_dataset(
#'   title = "Des données statistiques",
#'   description = "Description des données statistiques",
#'   topic = "Transports",
#'   frequency = "annual",
#'   frequency_date = "2022-01-10",
#'   temporal_coverage_start = "2021-01-01",
#'   temporal_coverage_end = "2021-12-31"
#' )
add_or_update_dataset <- function(title,
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
                                  caution = NULL,
                                  quiet = NULL) {
  datasets <- list_datasets()

  if (nrow(datasets) > 0) {
    ds <- datasets %>% filter(.data$title == .env$title)
  }

  if (nrow(datasets) == 0 || nrow(ds) == 0) {
    result <- add_dataset(
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
    return(new_dido_dataset(result))
  }

  if (nrow(ds) > 1) {
    abort_not_one_ligne(ds)
  }

  if (nrow(ds) == 1) {
    dataset = get_dataset(ds)

    if (!missing(description)) dataset$description = description
    if (!missing(topic)) dataset$topic = topic
    if (!missing(tags)) dataset$tags = tags
    if (!missing(frequency)) dataset$frequency = frequency
    if (!missing(frequency_date)) dataset$frequency_date = frequency_date
    if (!missing(granularity)) dataset$granularity = granularity
    if (!missing(license)) dataset$license = license
    if (!missing(caution)) dataset$caution = caution
    if (!missing(temporal_coverage_start)) dataset$temporal_coverage$start = temporal_coverage_start
    if (!missing(temporal_coverage_end)) dataset$temporal_coverage$end = temporal_coverage_end

    result <- update_dataset(dataset)
  }
  new_dido_dataset(result)
}
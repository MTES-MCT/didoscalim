#' Ajoute ou met à jour un dataset
#'
#' @description
#' Cette fonction permet, par exemple, de modifier la période de couverture du
#' dataset ainsi que la date de prochaine mise à jour.
#'
#' Si plusieurs datasets ont le même titre, cette méthode retourne une erreur.
#'
#' Si vous devez mettre à jour des datasets, il est conseillé d'utiliser la
#' fonction `didoscalim_update_only()`, cf
#' `vignette("complements-sur-les-mises-a-jour")`
#'
#'
#' @inheritParams add_dataset
#'
#' @return un objet [dido_dataset()]. Ce dernier sert pour créer par la suite
#'   les datafiles, les millésimes et les attachments
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
                                  caution = NULL) {
  check_mandatory_arguments("title", "description", "topic", "frequency")
  datasets <- list_datasets() %>%
    filter(compare_title(.data$title, .env$title))

  if (nrow(datasets) == 0) {

    if (is_update_only()) {
      abort_update_only(title, "dataset")
    }

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
    return(invisible(new_dido_dataset(result)))
  }

  abort_if_not_one_line("datasets", message = c(x = glue::glue("Il y a plusieurs datasets avec le titre `{title}`.")))

  if (nrow(datasets) == 1) {
    dataset <- get_dataset(datasets[1, ])
    origin <- rlang::duplicate(dataset)

    dataset$title <- toString(title)
    if (!missing(description)) dataset$description <- toString(description)
    if (!missing(topic)) dataset$topic <- toString(topic)
    if (!missing(tags)) dataset$tags <- tags
    if (!missing(frequency)) dataset$frequency <- frequency
    if (!missing(frequency_date)) dataset$frequency_date <- frequency_date
    if (!missing(granularity)) dataset$spatial$granularity <- granularity
    if (!missing(zones)) dataset$spatial$zones <- if (class(zones)[[1]] == "list") zones else list(zones)
    if (!missing(license)) dataset$license <- license
    if (!missing(caution)) dataset$caution <- toString(caution)
    if (!missing(temporal_coverage_start)) dataset$temporal_coverage$start <- temporal_coverage_start
    if (!missing(temporal_coverage_end)) dataset$temporal_coverage$end <- temporal_coverage_end

    if (!identical(origin, dataset)) {
      dataset <- update_dataset(dataset)
    }

    invisible(new_dido_dataset(dataset))
  }
}

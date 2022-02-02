#' Met à jour les metadonnées d'un dataset
#'
#' Cette fonction permet de mettre à jour les métadonnées d'un dataset comme,
#' par exemple, la date de fin de couverture temporelle.
#'
#' @param dataset un objet dido_dataset modifié
#'
#' @return un objet `dido_dataset()`
#' @export
#'
#' @family dataset
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dataset <- list_datasets() %>%
#'   filter(title == "Un jeu de données de test") %>%
#'   get_dataset() %>%
#'   clean_metadata()
#'
#' dataset$temporal_coverage$start <- "2020-01-01"
#' dataset$temporal_coverage$end <- "2020-12-31"
#' update_dataset(dataset)
#'
#' update_dataset(dataset)
update_dataset <- function(dataset) {
  if (missing(dataset) || is.null(dataset)) abort_bad_argument("dataset")
  if (!is.dido_dataset(dataset)) abort_not_dataset()

  id <- dataset$id
  url <- glue::glue("/datasets/{id}")

  metadata <- internal_clean_metadata(dataset)

  body <- jsonlite::toJSON(metadata, pretty = TRUE, auto_unbox = TRUE, na = "null")
  response <- dido_api(method = "PUT", path = url, body = body)
  invisible(new_dido_dataset(response))
}

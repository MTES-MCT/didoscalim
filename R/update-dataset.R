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
#' dataset <- get_dataset(list_datasets()[1, ]) %>% clean_metadata()
#'
#' dataset$description <- "another description"
#' dataset$tags <- list("autocar", "biogaz")
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

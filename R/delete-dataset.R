#' Supprime un dataset
#'
#' @param dataset un identifiant de dataset
#'
#' @return TRUE
#' @export
#'
#' @examples
#' dataset <- add_dataset(
#'   title = "le titre du dataset",
#'   description = "la description du dataset",
#'   topic = "Transports",
#'   frequency = "unknown"
#' )
#' delete_dataset(dataset)
#' @export
#' @keywords internal
delete_dataset <- function(dataset) {
  if (missing(dataset)) abort_bad_argument("dataset")
  if (is.null(get_dataset_id(dataset))) abort_not_dataset()

  url <- glue::glue("/datasets/{get_dataset_id(dataset)}")
  dido_api(method = "DELETE", path = url)
  invisible(TRUE)
}

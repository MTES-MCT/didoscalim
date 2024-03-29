#' Supprime un dataset
#'
#' @param dataset un identifiant de dataset
#'
#' @return TRUE
#' @export
#'
#' @export
delete_dataset <- function(dataset) {
  check_mandatory_arguments("dataset")

  if (is.null(get_dataset_id(dataset))) abort_not_dataset()

  url <- glue::glue("/datasets/{get_dataset_id(dataset)}")
  dido_api(method = "DELETE", path = url)
  invisible(TRUE)
}

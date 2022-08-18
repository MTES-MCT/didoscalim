#' Supprime un datafile.
#'
#' @param datafile un objet datafile retourné par `get_datafile()`, `add_datafile()`
#'
#' @return TRUE
#'
#' @family datafile
#'
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' datafile <- list_datafiles() %>%
#'   filter(title == "Un fichier de données de test")
#'
#' delete_datafile(datafile)
delete_datafile <- function(datafile) {
  if (missing(datafile) || is.null(datafile)) abort_bad_argument("datafile")
  if (!is.dido_datafile(datafile)) abort_not_datafile()

  rid <- get_datafile_rid(datafile)
  id <- get_dataset_id(datafile)

  url <- glue::glue("/datasets/{id}/datafiles/{rid}")

  result <- dido_api(method = "DELETE", path = url)
  invisible(TRUE)
}

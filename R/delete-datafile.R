#' Supprime un datafile.
#'
#' @param datafile un objet datafile retourné par `get_datafile()`, `add_datafile()`
#'
#' @return TRUE
#'
#' @family datafile
#'
#' @export
delete_datafile <- function(datafile) {
  abort_on_mandatory_argument(datafile, "datafile")

  if (!is.dido_datafile(datafile)) abort_not_datafile()

  rid <- get_datafile_rid(datafile)
  id <- get_dataset_id(datafile)

  url <- glue::glue("/datasets/{id}/datafiles/{rid}")

  result <- dido_api(method = "DELETE", path = url)
  invisible(TRUE)
}

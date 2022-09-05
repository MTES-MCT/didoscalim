#' Supprimer un millésime
#'
#' @inheritParams add_millesime
#'
#' @return TRUE
#' @export
#'
#' @family millesime
#' @export
delete_millesime <- function(datafile,
                             millesime,
                             quiet = NULL) {
  check_mandatory_arguments("datafile", "millesime")

  if (is.null(get_datafile_rid(datafile))) abort_not_datafile()

  rid <- get_datafile_rid(datafile)
  id <- get_dataset_id(datafile)

  didoscalim_info(glue::glue("suppression du millesime `{id}` `{rid}` `{millesime}`"))

  url <- glue::glue("/datasets/{id}/datafiles/{rid}/millesimes/{millesime}")

  job <- dido_api(method = "DELETE", path = url)
  invisible(TRUE)
}

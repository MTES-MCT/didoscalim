#' Supprimer un millésime
#'
#' @inheritParams add_millesime
#' @param quiet quand TRUE ou que l'option dido_quiet est à TRUE supprime les
#'   messages d'information, `NULL` par défaut
#'
#' @return TRUE
#' @export
#'
#' @family millesime
#' @export
delete_millesime <- function(datafile,
                              millesime,
                              quiet = NULL) {
  abort_on_mandatory_argument(datafile, "datafile")
  abort_on_mandatory_argument(millesime, "millesime")

  if (is.null(get_datafile_rid(datafile))) abort_not_datafile()

  rid <- get_datafile_rid(datafile)
  id <- get_dataset_id(datafile)

  didoscalim_info(glue::glue("suppression du millesime `{id}` `{rid}` `{millesime}`"))

  url <- glue::glue("/datasets/{id}/datafiles/{rid}/millesimes/{millesime}")

  job <- dido_api(method = "DELETE", path = url)
  invisible(TRUE)
}

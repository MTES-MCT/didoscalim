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
  if (missing(datafile)) abort_bad_argument("datafile")
  if (is.null(get_datafile_rid(datafile))) abort_not_datafile()

  if (missing(millesime) || is.null(millesime)) abort_bad_argument("millesime")

  rid <- get_datafile_rid(datafile)
  id <- get_dataset_id(datafile)

  if (!is_quiet(quiet)) rlang::inform(message = glue::glue("suppression du millesime `{id}` `{rid}` `{millesime}`"))

  url <- glue::glue("/datasets/{id}/datafiles/{rid}/millesimes/{millesime}")

  job <- dido_api(method = "DELETE", path = url)
  invisible(TRUE)
}

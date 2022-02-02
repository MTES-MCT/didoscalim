#' Mettre à jour un datafile.
#'
#' @param datafile un objet datafile retourné par `get_datafile()` modifié par
#'   l'utilisateur
#'
#' @return un objet dido_job()` invisible
#'
#' @family datafile
#'
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' datafile <- list_datafiles() %>%
#'   filter(title == "Un fichier de données de test") %>%
#'   get_datafile() %>%
#'   clean_metadata()
#'
#' datafile$temporal_coverage$start <- "2022-01-01"
#' datafile$temporal_coverage$end <- "2023-12-31"
#'
#' update_datafile(datafile)
update_datafile <- function(datafile) {
  if (missing(datafile) || is.null(datafile)) abort_bad_argument("datafile")
  if (!is.dido_datafile(datafile)) abort_not_datafile()

  rid <- get_datafile_rid(datafile)
  id <- get_dataset_id(datafile)

  metadata <- internal_clean_metadata(datafile)
  if (is.null(metadata$published)) metadata$published <- format(Sys.time(), "%Y-%m-%d")

  url <- glue::glue("/datasets/{id}/datafiles/{rid}/metadata")
  body <- jsonlite::toJSON(metadata, pretty = TRUE, auto_unbox = TRUE, na = "null")

  result <- dido_api(method = "PUT", path = url, body = body)
  attr(result, "id") <- id
  invisible(new_dido_datafile(result))
}

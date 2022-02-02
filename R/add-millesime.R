#' Ajoute un millésime à un datafile
#'
#' @param datafile un objet dido_datafile obtenu par `get_datafile()`
#' @inheritParams add_datafile
#'
#' @return un objet `dido_job()`
#' @export
#'
#' @family millesime
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' datafile <- list_datafiles() %>%
#'   filter(title == "Un fichier de données de test")
#'
#' millesime <- add_millesime(
#'   datafile = datafile,
#'   file_name = dido_example("augmente.csv"),
#'   millesime = "2011-10"
#' )
#'
#'
#' # publier un millésime avec embargo
#' millesime <- add_millesime(
#'   datafile = datafile,
#'   file_name = dido_example("augmente.csv"),
#'   millesime = "2012-10",
#'   date_diffusion = "2011-10-10 08:00:00"
#' )
add_millesime <- function(datafile,
                          file_name,
                          date_diffusion = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                          millesime = format(Sys.time(), "%Y-%m"),
                          quiet = NULL) {
  if (missing(datafile) || is.null(datafile)) abort_bad_argument("datafile")
  if (missing(file_name)) abort_bad_argument("file_name")
  if (is.null(get_datafile_rid(datafile))) abort_not_datafile()

  if (!is_quiet(quiet)) rlang::inform(message = glue::glue("    intégration du fichier `{file_name}`"))
  token_file <- dido_upload_file(file_name)
  if (!is_quiet(quiet)) rlang::inform(message = glue::glue("\t* fichier versé"))
  check_csv(token_file)
  if (!is_quiet(quiet)) rlang::inform(message = glue::glue("\t* fichier validé"))

  payload <- list(
    "tokenFile" = token_file,
    date_diffusion = date_heure_iso8601(date_diffusion),
    millesime = millesime
  )

  rid <- get_datafile_rid(datafile)
  id <- get_dataset_id(datafile)

  url <- glue::glue("/datasets/{id}/datafiles/{rid}")
  body <- jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE, na = "null")

  job <- dido_api(method = "POST", path = url, body = body)
  job_result <- dido_job(wait_for_job(job$id))

  if (!is_quiet(quiet)) {
    rlang::inform(glue::glue(
      "\t* fichier intégré",
      "\t    rid: {get_datafile_rid(job_result)}",
      "\t    millesime: {job_result$result$millesime}",
      "\t    lignes: {job_result$result$rows}",
    ))
  }

  invisible(job_result)
}

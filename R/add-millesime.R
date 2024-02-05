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
#'   slice(1) %>%
#'   get_datafile()
#'
#' millesime <- add_millesime(
#'   datafile = datafile,
#'   file_name = dido_example("augmente.csv"),
#'   millesime = "2011-10"
#' )
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
                          millesime = format(Sys.time(), "%Y-%m")) {
  check_mandatory_arguments("datafile", "file_name")

  if (is.null(get_datafile_rid(datafile))) abort_not_datafile()

  didoscalim_info(glue::glue("    intégration du fichier `{file_name}`"))
  token_file <- dido_upload_file(file_name)
  didoscalim_info(glue::glue("\t* fichier versé"))

  payload <- list(
    "tokenFile" = token_file,
    date_diffusion = date_heure_iso8601(date_diffusion),
    millesime = millesime
  )

  rid <- get_datafile_rid(datafile)
  id <- get_dataset_id(datafile)

  url <- glue::glue("/datasets/{id}/datafiles/{rid}")
  body <- jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE, na = "null")

  tryCatch(
    {
      job <- dido_api(method = "POST", path = url, body = body)
      job_result <- dido_job(wait_for_job(job$id))
    },
    error = function(cnd) {
      if (grepl("millésime.*existe déjà", cnd$message)) {
        class <- c("millesime_exists", class(cnd))
        abort(cnd$message, class = class, call = caller_env())
      } else {
        didoscalim_abort(parent = cnd)
      }
    }
  )

  didoscalim_info(glue::glue(
    "\t* fichier intégré: ",
    "\t rid: {get_datafile_rid(job_result)}",
    "\t millesime: {job_result$result$millesime}",
    "\t lignes: {job_result$result$rows}",
  ))

  invisible(job_result)
}

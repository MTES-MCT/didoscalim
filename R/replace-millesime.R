#' Remplacer les données d'un millésime
#'
#' Cette méthode remplace les données d'un millésime en conservant son identifiant.
#'
#' @inheritParams add_millesime
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
#' millesime <- replace_millesime(
#'   datafile = datafile,
#'   file_name = dido_example("augmente.csv"),
#'   millesime = "2021-12"
#' )
replace_millesime <- function(datafile,
                              file_name,
                              millesime,
                              date_diffusion = format(Sys.time(), "%Y-%m-%dT%H:00:00.000Z"),
                              quiet = NULL) {
  check_mandatory_arguments("datafile", "millesime", "file_name")

  if (is.null(get_datafile_rid(datafile))) abort_not_datafile()

  didoscalim_info(glue::glue("    intégration du fichier `{file_name}`"))
  token_file <- dido_upload_file(file_name)
  didoscalim_info(glue::glue("\t* fichier versé"))
  check_csv(token_file)
  didoscalim_info(glue::glue("\t* fichier validé"))

  payload <- list(
      "tokenFile" = token_file,
      date_diffusion = date_heure_iso8601(date_diffusion)
  )

  rid <- get_datafile_rid(datafile)
  id <- get_dataset_id(datafile)

  url <- glue::glue("/datasets/{id}/datafiles/{rid}/millesimes/{millesime}")
  body <- jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE, na = "null")

  job <- dido_api(method = "PUT", path = url, body = body)
  job_result <- dido_job(wait_for_job(job$id))

  didoscalim_info(glue::glue(
      "        * fichier intégré\n",
      "(rid: {job_result$result$rid}, ",
      "millesime: {job_result$result$millesime}, ",
      "lignes: {job_result$result$rows}"
  ))

  job_result
}

#' Ajoute un datafile à un dataset
#'
#' @param dataset l'id d'un dataset, un objet `dido_dataset()`,
#'   `dido_datafile()` ou `dido_job()`
#' @param title le titre du datafile
#' @param description la description du datafile
#' @param millesime le millesime (AAAA-MM). Par défaut AAAA-MM avec l'année
#'   courante et le mois courant
#' @param published la date de publication du fichier, si non précisée, prend la
#'   date du jour.
#' @param temporal_coverage_start optionnel, la date de début de couverture du
#'   fichier de données au format AAAA-MM-JJ
#' @param temporal_coverage_end optionnel, la date de fin de couverture du
#'   fichier de données au format AAAA-MM-JJ
#' @param legal_notice les mentions légales, par défaut "SDES"
#' @param date_diffusion (optionnel) la date/heure à laquelle le fichier
#'   devra être rendu accessible à la diffusion au format "AAAA-MM-JJ HH:MM:SS".
#'   Si la time zone n'est pas précisée, l'heure locale est utilisée.
#'
#'   Si cette date est dans le passé, les données sont immédiatement
#'   accessibles, si elle est dans le futur, les données ne seront accessibles
#'   qu'à cette date/heure.
#'
#'   Si non précisée prend la date/heure courante (heure locale), les données
#'   sont donc immédiatement accessibles.
#' @param file_name le nom du fichier à charger
#' @param quiet quand `TRUE` ou que l'option dido_quiet est à `TRUE` supprime
#'   les messages d'information, `NULL` par défaut
#'
#' @return un objet `dido_job()`
#'
#' @family datafile
#'
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dataset <- add_or_update_dataset(
#'   title = "Un dataset pour les datafiles",
#'   description = "Description des données statistiques",
#'   topic = "Transports",
#'   frequency = "unknown",
#' )
#'
#' add_datafile(
#'   dataset = dataset,
#'   title = "titre",
#'   description = "description",
#'   file_name = dido_example("augmente.csv")
#' )
#'
#' # publier un fichier de données avec toutes les métadonnées et un embargo
#' add_datafile(
#'   dataset = dataset,
#'   title = "titre 2",
#'   description = "description 2",
#'   file_name = dido_example("augmente.csv"),
#'   temporal_coverage_start = "2021-01-01",
#'   temporal_coverage_end = "2021-12-31",
#'   legal_notice = "something",
#'   millesime = "2020-10",
#'   date_diffusion = "2020-11-01 08:00:00"
#' )
add_datafile <- function(dataset,
                         title,
                         description,
                         file_name,
                         millesime = format(Sys.time(), "%Y-%m"),
                         published = format(Sys.time(), "%Y-%m-%d"),
                         temporal_coverage_start = NULL,
                         temporal_coverage_end = NULL,
                         legal_notice = "SDES",
                         date_diffusion = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                         quiet = NULL) {
  datafile <- dido_datafile(
    dataset = dataset,
    title = title,
    description = description,
    millesime = millesime,
    published = published,
    temporal_coverage_start = temporal_coverage_start,
    temporal_coverage_end = temporal_coverage_end,
    legal_notice = legal_notice,
    date_diffusion = date_diffusion
  )

  abort_on_mandatory_argument(file_name, "file_name")

  didoscalim_info(glue::glue("    intégration du fichier `{file_name}`"))
  datafile$tokenFile <- dido_upload_file(file_name)
  didoscalim_info(glue::glue("\t* fichier versé"))
  check_csv(datafile$tokenFile)
  didoscalim_info(glue::glue("\t* fichier validé"))

  df <- clean_metadata(datafile)

  body <- jsonlite::toJSON(df, pretty = TRUE, auto_unbox = TRUE, na = "null")

  id <- get_dataset_id(dataset)

  url <- glue::glue("/datasets/{id}/datafiles")
  job <- dido_job(dido_api(method = "POST", path = url, body = body))
  job_result <- dido_job(wait_for_job(job))

  didoscalim_info(glue::glue(
      "\t* fichier intégré",
      "\t    rid: {get_datafile_rid(job_result)}",
      "\t    millesime: {job_result$result$millesime}",
      "\t    lignes: {job_result$result$rows}",
  ))
  invisible(job_result)
}

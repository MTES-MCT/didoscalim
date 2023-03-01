#' Ajoute un fichier annexe à un dataset
#'
#' @param dataset l'id d'un dataset de rattachement, un objet `dido_dataset()`,
#'   `dido_datafile()` ou `dido_job()`
#' @param title le titre du fichier annexe
#' @param description la description du fichier annexe
#' @param file_name le nom du fichier à verser
#' @param type le type de fichier versé. Peut-être `documentation` ou
#'   `historical_data`. Par défaut `publication`
#' @param published la date/heure de publication du fichier, si non précisée, prend la
#'   date/heure du moment.
#'
#'   Ce paramètre est au format "AAAA-MM-JJ HH:MM:SS" (ou ISO8601 si vous
#'   préférez). Si la timezone n'est pas précisée, la timezone de l'ordinateur
#'   local est utilisée.
#'
#' @return un objet `dido_attachment()`
#' @export
#'
#' @family attachment
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dataset <- add_or_update_dataset(
#'   title = "Des données statistiques",
#'   description = "Description des données statistiques",
#'   topic = "Transports",
#'   frequency = "unknown",
#' )
#'
#' dataset %>% add_attachment(
#'   title = "title",
#'   description = "description",
#'   file_name = dido_example("attachment.txt")
#' )
add_attachment <- function(dataset,
                           title,
                           description,
                           file_name,
                           type = "documentation",
                           published = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                           quiet = NULL) {
  check_mandatory_arguments("dataset", "title", "description", "file_name")

  if (is.null(get_dataset_id(dataset))) abort_not_dataset()

  didoscalim_info(glue::glue("    intégration du fichier annexe `{basename(file_name)}`"))

  file_id <- dido_upload_file(file_name)
  didoscalim_info(glue::glue("\t* fichier versé"))

  payload <- list(
    title = title,
    description = description,
    tokenFile = file_id,
    published = published,
    type = type
  )

  id <- get_dataset_id(dataset)

  url <- glue::glue("/datasets/{id}/attachments")
  result <- dido_api(
    method = "POST",
    path = url,
    body = jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE, na = "null")
  )
  didoscalim_info(glue::glue("\t* fichier annexe intégré (rid: {result$rid})"))
  attr(result, "id") <- id

  invisible(dido_attachment(result))
}

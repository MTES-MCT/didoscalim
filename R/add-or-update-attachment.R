#' Ajoute ou modifie un fichier annexe dans un dataset
#'
#' met à jour l'attachement avec le même titre s'il existe sinon ajoute un attachement
#'
#' @inheritParams add_attachment
#' @param check_file_date TRUE/FALSE, Si TRUE met à jour l'attachment uniquement
#'   si le fichier est plus récent que le last_modified de l'attachment
#'
#' @return un objet `dido_attachment()` ou `NULL` si aucune création/mise à jour
#'   n'a eu lieu
#'
#' @export
#'
#' @family attachment
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dataset <- add_or_update_dataset(
#'   title = "Un dataset pour les attachement",
#'   description = "Description des données statistiques",
#'   topic = "Transports",
#'   frequency = "unknown",
#' )
#'
#' add_or_update_attachment(
#'   dataset = dataset,
#'   title = "title",
#'   description = "description",
#'   file_name = dido_example("attachment.txt")
#' )
add_or_update_attachment <- function(dataset,
                                     title,
                                     description,
                                     file_name,
                                     published = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                     check_file_date = FALSE,
                                     quiet = NULL) {
  check_mandatory_arguments("dataset", "title", "description", "file_name")

  attachments <- dataset %>%
    list_attachments() %>%
    filter(.data[["title"]] == .env[["title"]])

  if (nrow(attachments) == 0) {
    dido_att <- add_attachment(
      dataset = dataset,
      title = title,
      description = description,
      file_name = file_name,
      published = published,
      quiet = quiet
    )
    return(invisible(dido_att))
  }

  abort_if_not_one_line("attachments", message = c(x = glue::glue("Il y a plusieurs attachements avec le titre `{title}`.")))

  if (nrow(attachments) == 1) {
    attachment <- get_attachment(attachments[1, ])

    if (check_file_date) {
      datafile_last_modified <- lubridate::as_datetime(attachment$last_modified)
      file_mtime <- lubridate::as_datetime(file.info(file_name)$mtime)
      if (file_mtime < datafile_last_modified) {
        return(NULL)
      }
    }

    dido_att <- replace_attachment(attachments[1, ], file_name)

    if (!missing(description)) attachment$description <- description
    if (!missing(published)) attachment$published <- published
    attachment <- update_attachment(attachment)

    return(invisible(attachment))
  }
}

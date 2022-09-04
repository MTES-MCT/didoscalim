#' Ajoute ou modifie un fichier annexe dans un dataset
#'
#' met à jour l'attachement avec le même titre s'il existe sinon ajoute un attachement
#'
#' @inheritParams add_attachment
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
                                     published = format(Sys.time(), "%Y-%m-%d"),
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
    dido_att <- replace_attachment(attachments[1,], file_name)
    attachment <- get_attachment(attachments[1,])
    if (!missing(description)) attachment$description <- description
    if (!missing(published)) attachment$published <- published
    attachment <- update_attachment(attachment)

    return(invisible(attachment))
  }

}

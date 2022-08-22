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
  attachments <- dataset %>% list_attachments()

  if (nrow(attachments) > 0) {
    att <- attachments %>% filter(.data[["title"]] == .env[["title"]])
  }
  if (nrow(attachments) == 0 || nrow(att) == 0) {
    dido_att <- add_attachment(
      dataset = dataset,
      title = title,
      description = description,
      file_name = dido_example("attachment.txt"),
      published = published,
      quiet = quiet
    )
  return(invisible(dido_att))
  }

  if (nrow(att) > 1) {
    abort_not_one_ligne(att)
  }

  if (nrow(att) == 1) {
    dido_att <- replace_attachment(att, file_name)
    attachment = get_attachment(att)
    if (!missing(description)) attachment$description = description
    if (!missing(published)) attachment$published = published
    update_attachment(attachment)

    return(invisible(attachment))
  }

}

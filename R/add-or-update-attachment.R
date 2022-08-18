#' Ajoute ou modifie un un fichier annexe dans un dataset
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
#' dataset <- list_datasets() %>%
#'   filter(title == "Un jeu de données de test")
#'
#' add_attachment(
#'   dataset = dataset,
#'   title = "title",
#'   description = "description",
#'   file_name = dido_example("attachment.txt")
#' )
#'
#' # ou sans passer par une variable intermédiaire
#' list_datasets() %>%
#'   filter(title == "Un jeu de données de test") %>%
#'   add_attachment(
#'     title = "title",
#'     description = "description",
#'     file_name = dido_example("attachment.txt")
#'   )
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

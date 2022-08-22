#' Supprime un attachment
#'
#' @param attachment un objet attachment retourné par `get_attachment()`, `add_attachment()`
#'
#' @return TRUE
#'
#' @family attachment
#'
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dataset <- add_or_update_dataset(
#'   title = "Un dataset pour les attachements",
#'   description = "Description des données statistiques",
#'   topic = "Transports",
#'   frequency = "unknown",
#' )
#'
#' attachment <- add_attachment(
#'   dataset = dataset,
#'   title = "title",
#'   description = "description",
#'   file_name = dido_example("attachment.txt")
#' )
#' delete_attachment(attachment)
delete_attachment <- function(attachment) {
  if (missing(attachment) || is.null(attachment)) abort_bad_argument("attachment")
  if (!is.dido_attachment(attachment)) abort_not_attachment()

  rid <- get_attachment_rid(attachment)
  id <- get_dataset_id(attachment)

  url <- glue::glue("/datasets/{id}/attachments/{rid}")

  result <- dido_api(method = "DELETE", path = url)
  invisible(TRUE)
}

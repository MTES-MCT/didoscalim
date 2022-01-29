#' Mettre à jour une pièce jointe.
#'
#' @param attachment un objet attachment retourné par `get_attachment()` modifié
#'   par l'utilisateur
#'
#' @return un objet `dido_attachment()`
#'
#' @family attachment
#'
#' @export
#'
#' @examples
#' att <- list_attachments()[1,]
#' dataset <- att$id
#' attachment <- get_attachment(list_attachments()[1,], dataset = dataset)
#' attachment$title <- "un nouveau titre"
#' update_attachment(attachment)
update_attachment <- function(attachment) {
  if (missing(attachment) || is.null(attachment)) abort_bad_argument("attachment")
  if (!is.dido_attachment(attachment)) abort_not_attachment()

  rid <- get_attachment_rid(attachment)
  id <- get_dataset_id(attachment)

  metadata <- internal_clean_metadata(attachment)
  if (is.null(metadata$published)) metadata$published <- format(Sys.time(), "%Y-%m-%d")

  url <- glue::glue("/datasets/{id}/attachments/{rid}/metadata")

  body <- jsonlite::toJSON(metadata, pretty = TRUE, auto_unbox = TRUE, na = "null")

  result <- dido_api(method = "PUT", path = url, body = body)
  attr(result, "id") <- id
  invisible(dido_attachment(result))
}

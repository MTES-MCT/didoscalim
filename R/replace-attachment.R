#' Remplace un fichier annexe
#'
#' @param attachment un objet `dido_attachment()` ou un rid
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
#' attachment <- list_attachments(dataset) %>%
#'   filter(title == "Un autre fichier annexe")
#'
#' replace_attachment(
#'   attachment = attachment,
#'   file_name = dido_example("attachment.txt")
#' )
replace_attachment <- function(attachment,
                               file_name,
                               quiet = NULL) {
  if (missing(attachment) || is.null(attachment)) abort_bad_argument("attachment")
  if (is.null(get_attachment_rid(attachment))) abort_not_attachment()

  if (missing(file_name) || is.null(file_name)) abort_bad_argument("file_name")

  if (!is_quiet(quiet)) rlang::inform(message = glue::glue("    intégration du fichier annexe `{file_name}`"))

  file_id <- dido_upload_file(file_name)
  if (!is_quiet(quiet)) rlang::inform(message = glue::glue("\t* fichier versé"))

  payload <- list(
    "tokenFile" = file_id
  )

  rid <- get_attachment_rid(attachment)
  id <- get_dataset_id(attachment)

  url <- glue::glue("/datasets/{id}/attachments/{rid}/file")
  result <- dido_api(
    method = "PUT",
    path = url,
    body = jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE, na = "null")
  )
  if (!is_quiet(quiet)) rlang::inform(glue::glue("\t* fichier annexe remplacé (rid: {result$rid})"))
  attr(result, "id") <- id

  invisible(dido_attachment(result))
}

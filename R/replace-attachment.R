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
                               file_name = NULL,
                               remote_url = NULL) {
  check_mandatory_arguments("attachment")

  if (is.null(remote_url) & is.null(file_name)) {
    rlang::abort("error_bad_argument", message = "un des arguments remote_url ou file_name est obligatoire")
  }

  if (is.null(get_attachment_rid(attachment))) abort_not_attachment()

  payload <- list()

  if (!is.null(file_name)) {
    didoscalim_info(glue::glue("    intégration du fichier annexe `{file_name}`"))

    file_id <- dido_upload_file(file_name)
    didoscalim_info(glue::glue("\t* fichier versé"))

    payload$tokenFile = file_id
  } else {
    payload$remoteUrl = remote_url
  }

  rid <- get_attachment_rid(attachment)
  id <- get_dataset_id(attachment)

  url <- glue::glue("/datasets/{id}/attachments/{rid}/file")
  result <- dido_api(
    method = "PUT",
    path = url,
    body = jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE, na = "null")
  )

  if (!is.null(remote_url)) {
    didoscalim_info(glue::glue("\t* fichier annexe remplacé (rid: {result$rid})"))
  }
  attr(result, "id") <- id

  invisible(dido_attachment(result))
}

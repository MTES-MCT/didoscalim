#' Récupère les métadonnées d'un attachment
#'
#' Permet de récupérer les données d'un attachment
#'
#' @param data le résultat d'une recherche par `list_attachments()`, un rid de
#'   attachment, un objet `dido_attachment()` ou `dido_job()`
#' @param dataset optionnel, le dataset parent de l'attachement.
#'
#' @return un objet `dido_attachment()`
#'
#' @family attachment
#'
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts=FALSE)
#'
#' attachment <- list_attachments() %>%
#'   filter(title == "Un fichier annexe") %>%
#'   get_attachment()
get_attachment <- function(data = NULL, dataset = NULL) {
  if (is.null(get_attachment_rid(data))) abort_not_attachment()

  rid <- get_attachment_rid(data)
  dataset_id <- get_dataset_id(data) %||%
    get_dataset_id(dataset) %||%
    get_attachment_id_by_rid(rid)

  url <- glue::glue("/datasets/{dataset_id}/attachments/{rid}")
  result <- dido_api(method = "GET", path = url)

  attr(result, "id") <- dataset_id

  new_dido_attachment(result)
}

get_attachment_id_by_rid <- function(rid) {
  find_by_column(data = list_attachments(), string = stringr::fixed(rid), col = "rid")
}

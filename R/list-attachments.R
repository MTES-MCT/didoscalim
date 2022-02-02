#' Liste les attachments
#'
#' Retourne la liste des attachments ou si un objet dataset est passé en argument,
#' la liste des attachments de ce dataset.
#'
#' @param dataset optionnel, un objet dataset
#'
#' @return un tibble avec les attachments
#'
#' @family attachment
#'
#' @export
#'
#' @examples
#' # toutes les pièces jointes
#' list_attachments()
#'
#' library(dplyr, warn.conflicts = FALSE)
#'
#' # les pièces jointes d'un dataset en particulier
#' list_datasets() %>%
#'   filter(title == "Un jeu de données de test") %>%
#'   list_attachments()
list_attachments <- function(dataset = NULL) {
  if (!is.null(dataset) && is.null(get_dataset_id(dataset))) {
    abort_not_dataset()
  }
  ds <- list_datasets()
  if (nrow(ds) == 0) {
    return(tibble())
  }
  if (!is.null(dataset)) ds <- filter(ds, .data$id == get_dataset_id(dataset))
  df <- dplyr::select(ds, .data$id, .data$attachments)
  as_tibble(tidyr::unnest(df, .data$attachments))
}

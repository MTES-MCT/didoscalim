#' Liste les datafiles
#'
#' Retourne la liste des datafiles ou si un objet dataset est passé en argument,
#' la liste des datafiles de ce dataset.
#'
#' @param dataset optionnel, un objet dataset
#'
#' @return un tibble avec les datafiles
#'
#' @family datafile
#'
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#' # tous les datafiles
#' list_datafiles()
#'
#' # les datafiles d'un jeu en particulier
#' list_datasets() %>%
#'   filter(title == "Un jeu de données de test") %>%
#'   list_datafiles()
list_datafiles <- function(dataset = NULL) {
  if (!is.null(dataset) && is.null(get_dataset_id(dataset))) {
    abort_not_dataset()
  }
  ds <- list_datasets()
  if (nrow(ds) == 0) {
    return(tibble() %>% add_columns_if_empty(c("id", "rid", "title", "description")))
  }
  if (!is.null(dataset)) ds <- filter(ds, .data$id == get_dataset_id(dataset))

  df <- dplyr::select(ds, .data$id, .data$datafiles)
  as_tibble(tidyr::unnest(df, .data$datafiles)) %>%
    add_columns_if_empty(c("id", "rid", "title", "description"))
}

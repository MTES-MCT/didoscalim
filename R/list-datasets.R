#' Récupère la liste des datasets
#'
#' @return un tibble des datasets de l'utilisateur
#' @export
#'
#' @examples
#' list_datasets()
list_datasets <- function() {
  url <- "/datasets"
  dido_api(method = "GET", path = url, as_tibble = TRUE) %>%
    add_columns_if_empty(c("id", "title", "description"))
}

#' Récupère les métadonnées d'un dataset
#'
#' Permet de récupérer les métadonnées d'un dataset en utilisant la sortie de la
#' fonction `list_datasets()` ou un des objets `dido_datafile()`,
#' `dido_dataset()`, `dido_job()`
#'
#' Lève une exception si `list_datasets()` fourni en entrée retourne plus d'un
#' dataset.
#'
#' @param data la sortie d'une commande `list_datasets()`, un objet
#'   `dido_dataset()`, `dido_job()` ou `dido_datafile()` ou l'identifiant d'un
#'   dataset
#'
#' @return un objet `dido_dataset()`
#' @export
#'
#' @family dataset
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' ds <- list_datasets() %>%
#'   filter(title == "Données de consommation fictive") %>%
#'   get_dataset()
get_dataset <- function(data) {
  if (missing(data)) {
    msg <- glue::glue("Vous devez préciser l'argument `data`")
    rlang::abort("error_bad_argument", message = msg)
  }
  if (is.null(get_dataset_id(data))) abort_not_dataset()

  id <- get_dataset_id(data)

  url <- glue::glue("/datasets/{id}")
  result <- dido_api(method = "GET", path = url)
  new_dido_dataset(result)
}

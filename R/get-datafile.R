#' Récupère un datafile
#'
#' Récupère les métadonnées d'un datafile
#'
#' Lève une exception si `list_datasets()` fourni en entrée retourne plus d'un
#' dataset.
#'
#' @param data le résultat d'une recherche par `list_datafiles()`, un des objets
#'   `dido_job()` `dido_datafile()` oule rid d'un datafile
#' @param dataset optionnel l'identifiant d'un dataset ou un objet
#'   `dido_dataset()`
#'
#' @return un objet [dido_datafile]
#'
#' @family datafile
#'
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' datafile <- list_datafiles() %>%
#'   slice(1) %>%
#'   get_datafile(datafile)
get_datafile <- function(data = NULL, dataset = NULL) {
  if (is.null(get_datafile_rid(data))) abort_not_datafile()

  rid <- get_datafile_rid(data)
  dataset_id <- get_dataset_id(data) %||%
    get_dataset_id(dataset) %||%
    get_datafile_id_by_rid(rid)

  url <- glue::glue("/datasets/{dataset_id}/datafiles/{rid}")
  result <- dido_api(method = "GET", path = url)

  attr(result, "id") <- dataset_id

  new_dido_datafile(result)
}

get_datafile_id_by_rid <- function(rid) {
  find_by_column(data = list_datafiles(), string = stringr::fixed(rid), col = "rid")
}

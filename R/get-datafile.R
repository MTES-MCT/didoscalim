#' Récupère un datafile
#'
#' Permet de récupérer les données d'un datafile en utilisant soit son rid soit
#' son titre.
#'
#' Lève une exception si la recherche ne retourne plus ou moins que 1 datafile
#'
#' @param data le résultat d'une recherche par `list_datafiles()`, un rid de
#'   datafile, un objet `dido_job()` ou `dido_datafile()`
#' @param dataset optionnel l'identifiant du dataset ou un objet
#'   `dido_dataset()`
#'
#' @return un objet [dido_datafile()]
#'
#' @family datafile
#'
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts=FALSE)
#'
#' datafile <- list_datafiles() %>%
#'   filter(title == "Un fichier de données de test") %>%
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

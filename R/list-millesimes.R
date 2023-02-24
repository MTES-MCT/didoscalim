#' Liste les millesimes
#'
#' Retourne un tibble de tous les millésimes ou d'un datafile en particulier
#'
#' @inheritParams get_datafile
#'
#' @return un tibble avec les millesimes du datafile
#' @export
#'
#' @family millesime
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' datafile <- list_datafiles() %>%
#'   filter(title == "Un fichier de données de test")
#'
#' list_millesimes(datafile)
list_millesimes <- function(data = NULL) {
  if (!is.null(data) && is.null(get_datafile_rid(data))) abort_not_datafile()

  df <- list_datafiles()

  if (!is.null(data)) df <- filter(df, .data$rid == get_datafile_rid(data))

  ml <- dplyr::select(df, "id", "rid", "millesimes_info")
  as_tibble(tidyr::unnest(ml, "millesimes_info"))
}

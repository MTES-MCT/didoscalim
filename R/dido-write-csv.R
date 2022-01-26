#' Enregistre le fichier CSV augmenté utilisé par DiDo.
#'
#' @param data un tibble retourné par `dido_csv()`
#' @param file le nom du fichier
#'
#' @return le tibble passé en entrée
#'
#' @export
#'
#' @examples
#' \dontrun{
#' write_dido_csv(data, "/tmp/fichier.csv")
#' }
dido_write_csv <- function(data, file) {
  readr::write_delim(data,
                     file,
                     delim = ";",
                     na = "",
                     col_names = FALSE,
                     quote = "all"
  )
}

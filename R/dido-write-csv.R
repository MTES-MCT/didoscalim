#' Enregistre le fichier CSV augmenté généré
#'
#' @param data un tibble retourné par `dido_csv()`
#' @param file le nom du fichier
#'
#' @return le tibble passé en entrée
#'
#' @family csv
#'
#' @export
#'
#' @examples
#' dido_read_delim(dido_example("exemple.csv")) %>%
#'   dido_csv() %>%
#'   dido_write_csv("/tmp/fichier.csv")
dido_write_csv <- function(data, file) {
  abort_on_mandatory_argument(data, "data")
  abort_on_mandatory_argument(file, "file")

  readr::write_delim(data,
    file,
    delim = ";",
    na = "",
    col_names = FALSE,
    quote = "all"
  )
}

#' Retourne le chemin du fichier exemple données ou le chemin des fichiers
#' exemple
#'
#' Les fichiers exemples sont stockés dans `inst/extdata`
#'
#' @param file le nom du fichier. Si `NULL` les exemples sont listés.
#' @export
#' @examples
#' dido_example()
#' dido_example("exemple.csv")
#
# code from readr: https://github.com/tidyverse/readr/blob/main/R/example.R
dido_example <- function(file = NULL) {
  if (is.null(file)) {
    dir(system.file("extdata", package = "didoscalim"))
  } else {
    system.file("extdata", file, package = "didoscalim", mustWork = TRUE)
  }
}

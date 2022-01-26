#' Lit un fichier CSV
#'
#' Cette fonction utilise directement `readr::read_delim` en enlevant la
#' détection du type des colonnes.
#'
#' @inheritParams readr::read_delim
#'
#' @return un tibble dont toutes les colonnes sont de type `chr`
#'
#' @details Certaines variables peuvent avoir des valeurs secrétisées
#'   représentées par la valeur `secret`, la détection automatique de readr
#'   n'est donc pas fiable et est désactivé à ce niveau. La détection
#'   automatique est faite dans la fonction `dido_csv()`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- dido_read_delim("vignettes/exemple.csv")
#' }
dido_read_delim <- function(file, delim = NULL, quote = '"',
                            escape_backslash = FALSE, escape_double = TRUE,
                            locale = readr::default_locale(),
                            comment = "", trim_ws = FALSE,
                            skip = 0, n_max = Inf,
                            skip_empty_rows = TRUE) {
  readr::read_delim(
    file = file,
    delim = delim,
    quote = quote,
    col_types = readr::cols(.default = "c"),
    escape_backslash = escape_backslash,
    escape_double = escape_double,
    locale = locale,
    comment = comment,
    trim_ws = trim_ws,
    skip = skip, n_max = n_max,
    skip_empty_rows = skip_empty_rows
  )
}

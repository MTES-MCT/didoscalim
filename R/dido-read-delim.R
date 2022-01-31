#' Lit un fichier CSV
#'
#' Cette fonction utilise directement `readr::read_delim` en enlevant la
#' détection du type des colonnes.
#'
#'
#' @param delim le séparateur de champ. Par défaut ";".
#' @param locale la locale à utiliser pour lire les fichiers. A ce niveau, le
#'   paramètre le plus important est l'encodage du fichier. Le défaut est
#'   "UTF-8". Les autres encodages fréquemment rencontrés sont "WINDOWS-1252" et
#'   "ISO-8859-15"
#' @inheritParams readr::read_delim
#'
#' @return un tibble dont toutes les colonnes sont de type `chr`
#'
#' @family csv
#'
#' @details Certaines variables peuvent avoir des valeurs secrétisées
#'   représentées par la valeur `secret`, la détection automatique du package
#'   `readr` n'est donc pas fiable et est désactivé à ce niveau. La détection
#'   automatique est faite dans la fonction `dido_csv()`.
#'
#' @export
#'
#' @examples
#' dido_read_delim(dido_example("exemple.csv"))
#' dido_read_delim(dido_example("csv-win-char.csv"),
#'   delim = ",",
#'   locale = locale(encoding = "WINDOWS-1252")
#' )
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

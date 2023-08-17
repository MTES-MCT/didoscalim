#' Lit un fichier CSV
#'
#' Cette fonction utilise directement `readr::read_delim` en enlevant la
#' détection du type des colonnes et en fixant `delim` à ";" par défaut.
#'
#'
#' @param delim le séparateur de champ. Par défaut ";".
#' @param locale la locale à utiliser pour lire les fichiers. A ce niveau, le
#'   paramètre le plus important est l'encodage du fichier. Le défaut est
#'   "UTF-8". Les autres encodages fréquemment rencontrés sont "WINDOWS-1252" et
#'   "ISO-8859-15"
#' @param ... ces arguments seront passés directement à la fonction
#'   `readr::read_delim()`
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
dido_read_delim <- function(file, delim = ";", ...) {
  readr::read_delim(
    file = file,
    delim = delim,
    col_types = readr::cols(.default = "c"),
    ...
  )
}

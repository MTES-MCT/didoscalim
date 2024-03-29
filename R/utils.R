#' Retourne une date au format ISO 8601 avec la TZ
#'
#' @param date_heure date/heure
#'
#' @return une date/heure au format ISO  8601 avec la TZ
#'
#' @export
#'
#' @examples
#' date_heure_iso8601("2022-02-02 07:00:02")
#' @keywords internal
date_heure_iso8601 <- function(date_heure) {
  lubridate::format_ISO8601(lubridate::ymd_hms(date_heure, tz = Sys.timezone()), usetz = "TRUE", quiet = TRUE)
}

#' @noRd
check_mandatory_arguments <- function(..., call = caller_env()) {
  for (name in list(...)) {
    arg <- env_get(call, name)
    if (missing(arg) || is.null(arg)) {
      msg <- glue::glue("`{name}` est obligatoire et ne peut être null")
      rlang::abort("error_bad_argument", message = msg, call = call)
    }
  }
  TRUE
}

#' @noRd
abort_not_dataset <- function() {
  message <- c(
    glue::glue("`data` n'est pas du type attendu"),
    i = glue::glue(
      "`data` doit être soit un id de dataset soit la valeur",
      "retournée par une des fonctions : `add_dataset`, `get_dataset`, ",
      "`get_datafile`, `add_datafile`, `list_datasets()`"
    )
  )

  rlang::abort("error_bad_argument_type", message = message)
}

#' @noRd
abort_not_datafile <- function() {
  message <- c(
    glue::glue("`data` n'est pas du type attendu"),
    i = glue::glue(
      "`data` doit être un rid de datafile ou la valeur ",
      "retournée par une des fonctions `get_datafile`, `add_datafile`, ",
      "`list_datafiles()`"
    )
  )

  rlang::abort("error_bad_argument_type", message = message)
}

#' @noRd
abort_not_attachment <- function() {
  message <- c(
    glue::glue("`data` n'est pas du type attendu"),
    i = glue::glue(
      "`data` doit être un rid d'attachment ou la valeur ",
      ", retournée par une des fonctions `add_attachment`, `get_attachment`, ",
      "`list_attachments()"
    )
  )

  rlang::abort("error_bad_argument_type", message = message)
}

#' @noRd
abort_not_job <- function() {
  message <- c(
    glue::glue("`job` n'est pas du type attendu"),
    i = glue::glue(
      "`job` doit être un id de job ou la valeur retournée par ",
      "la fonction `get_job`"
    )
  )

  rlang::abort("error_bad_argument_type", message = message)
}

#' cherche dans un tibble
#'
#' @param data le dataframe/tibble dans lequel rechercher
#' @param string la chaine à chercher
#' @param col la colonne dans laquelle chercher
#' @param return_col a vecteur de colonnes à retourner
#'
#' @return un id
#'
#' @examples
#' \dontrun{
#' find_by_column(data, "un titre", "title")
#' find_by_column(data = data, string = "un titre", "title", c("id", "rid"))
#' find_by_column(data = data, string = stringr::fixed("rid"), "rid")
#' }
#' @keywords internal
find_by_column <- function(data, string, col, return = c("id")) {
  if (nrow(data) == 0) {
    rlang::abort(glue::glue("no_data"), message = glue::glue("Impossible de trouver un `{return}` avec comme `{col}` `{string}`"))
  }
  founded <- filter(data, stringr::str_detect(string = .data[[col]], pattern = string))

  if (nrow(founded) > 1) {
    message <- c(
      glue::glue("la recherche `{string}` retourne {nrow(founded)} lignes(s)"),
      i = glue::glue("Votre chaine de recherche est-celle trop ou pas assez précise ?")
    )
    rlang::abort("too_many_data", message = message)
  }
  if (nrow(founded) == 0) {
    message <- c(
      glue::glue("la recherche `{string}` retourne {nrow(founded)} lignes(s)")
    )
    rlang::abort("no_data", message = message)
  }


  return(founded[return])
}

#' return TRUE if str match mongo oid regexp
#' @noRd
is_mongo_oid <- function(str) {
  stringr::str_detect(str, "^[0-9a-fA-F]{24,}$")
}

#' return TRUE if str match uuid regexp
#' @noRd
is_uuid <- function(str) {
  stringr::str_detect(str, "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$")
}

#' return an rowless tibble with columns if data is 0x0
#' @noRd
add_columns_if_empty <- function(data, columns = c()) {
  for (col in setdiff(columns, names(data))) data <- mutate(data, {{ col }} := character())
  data
}

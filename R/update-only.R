#' Bloquer les créations de datasets, datafiles et attachments
#'
#' @description
#' `didoscalim_update_only()` permet d'autoriser ou d'interdire la création
#' d'objets dans les fonctions `add_or_update_dataset()`,
#' `add_or_update_datafile()` et `add_or_update_attachment()` dans la suite du
#' programme
#'
#' @param bool TRUE/FALSE, si TRUE bloque les créations
#'
#' @return Le précédente valeur ou FALSE si non fixée
#'
#' @export
#' @examples
#' # bloque la création dans la suite du programme
#' didoscalim_update_only(TRUE)
#'
#' # autorise la création dans la suite du programme
#' didoscalim_update_only(FALSE)
didoscalim_update_only <- function(bool) {
  old_status <- is_update_only()

  withr::local_options(list(didoscalim_update_only = bool), .local_envir = parent.frame())

  didoscalim_info(c(i = glue("Création de nouveaux objets : {if (bool) 'interdite' else 'autorisée'}")))

  return(invisible(old_status))
}

is_update_only <- function() {
  getOption("didoscalim_update_only", FALSE)
}

abort_update_only <- function(title, type) {
  message <- c(
    glue::glue("aucun {type} ne correspond au titre '{title}'"),
    i = glue::glue(
      "l'option `didoscalim_update_only` est positionnée à `TRUE`."
    )
  )

  rlang::abort("error_update_only", message = message)
}

# "Inspired" (as in mainly copied) from https://github.com/r-lib/gargle
# Copyright (c) 2020 RStudio; Google Inc
# See https://github.com/r-lib/gargle/blob/main/LICENSE.md

#' Voir et modifier le niveau de message de didoscalim
#'
#' @description
#' `didoscalim_verbosity()` retourne l'option nommée "didoscalim_verbosity", qui
#' détermine la verbosité générale de didoscalim. Il y a trois niveaux possibles
#' :
#' * "debug": Niveau pour débugger
#' * "info" (défaut): Niveau normal d'information utile à l'utilisateur.
#' * "silent": Aucun message sauf les erreurs.
#'
#' Si vous souhaitez changer le niveau par défaut, il suffit d'utiliser `option`
#' :
#' ```
#' options(
#'   didoscalim_verbosity = "silent",
#' )
#' ```
#'
#' @export
#' @examples
#' didoscalim_verbosity()
#' options(
#'   didoscalim_verbosity = "silent"
#' )
#' didoscalim_verbosity()
didoscalim_verbosity <- function() {
  gv <- getOption("didoscalim_verbosity", "info")

  vals <- c("debug", "info", "silent")
  if (!is_string(gv) || !(gv %in% vals)) {
    # ideally this would collapse with 'or' not 'and' but I'm going with it
    didoscalim_abort('Option "didoscalim_verbosity" must be one of: {.field {vals}}')
  }
  gv
}

#' Journalisation et niveau de verbosité
#'
#' Des méthodes liées à la gestion de la verbosité et à la journalisation
#'
#' @name verbosity
#'
#' @export
#' @section `local_didoscalim_verbosity`:
#' change le niveau de verbosité dans la suite du code au même scope
local_didoscalim_verbosity <- function(level, env = parent.frame()) {
  withr::local_options(list(didoscalim_verbosity = level), .local_envir = env)
}

#' @rdname verbosity
#'
#' @export
#' @section `with_didoscalim_verbosity`:
#' change le niveau de verbosité dans le bloc de code
with_didoscalim_verbosity <- function(level, code) {
  withr::with_options(list(didoscalim_verbosity = level), code = code)
}

#' @rdname verbosity
#'
#' @export
#' @section `didoscalim_debug()`:
#' affiche les messages de debug
didoscalim_debug <- function(text, .envir = parent.frame()) {
  if (didoscalim_verbosity() == "debug") {
    cli::cli_bullets(text, .envir = .envir)
  }
}

#' @rdname verbosity
#'
#' @export
#' @section `didoscalim_info()`:
#' affiche les messages d'info
didoscalim_info <- function(text, .envir = parent.frame()) {
  if (didoscalim_verbosity() %in% c("debug", "info")) {
    rlang::inform(message = text)
  }
}

#' @rdname verbosity
#'
#' @export
#' @section `didoscalim_abort`:
#' affiche le message au niveau le plus elevé et lève une exception.
didoscalim_abort <- function(message = NULL, ...,
                             class = NULL,
                             .envir = parent.frame(),
                             call = caller_env()) {
  abort(
    message = message,
    class =  unique(c(class, "didoscalim_error")),
    .envir = .envir,
    call = call,
    ...
  )
}


abort_if_not_one_line <- function(name,
                                  message = NULL,
                                  .envir = parent.frame(),
                                  call = caller_env()) {
  data <- rlang::env_get(.envir, name)

  if (nrow(data) == 1) return(invisible(TRUE))

  if (missing(message) || is.null(message)) {
    message <- c(
      x = glue::glue("L'argument `{name}` doit contenir une ligne."),
      i = glue::glue("`{name}` contient {nrow(data)} ligne(s)."),
      i = glue::glue(
        "Avez-vous oublié de filtrer (avec dplyr::filter ",
        "par exemple`) le dataframe avant de le passer en", "
                 argument ?"
      )
    )
  }
  rlang::abort(message = message)
  didoscalim_abort(message,
                   class = "not_one_row",
                   .envir = .envir,
                   call = call)
}

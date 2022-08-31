.didoscalim_env <- new.env()
ordered_env_names <- c("DEV", "PREPROD", "ECOLE")

#' @noRd
missing_key_message <- function(env_name, key) {
  glue::glue(
    "La configuration de l'environnement {env_name}",
    " est incomplète\nil manque la clef {key}_{env_name}",
    " dans le .Renviron"
  )
}

#' Recharge les environnements
#'
#' Cette commande permet de recharger les environnements si vous avez modifié le Renviron.
#' Vous devez d'abord le relire avec [readRenviron()]
#'
#' @export
#'
#' @examples
#' readRenviron("~/.Renviron")
#' load_envs()
load_envs <- function() {
  rm(list = ls(pos = .didoscalim_env), pos = .didoscalim_env)

  e <- Sys.getenv()
  dido_env <- e[grep("DIDOSCALIM_(API_KEY|BASE_PATH)_.*", names(e))]
  config <- list()
  for (n in names(dido_env)) {
    v <- stringr::str_split(n, "_", 4)
    env <- v[[1]][4]
    name <- paste0(v[[1]][2:3], collapse = "_")

    config[[env]][[name]] <- dido_env[[n]]
  }
  for (env_name in names(config)) {
    if (is.null(config[[env_name]][["API_KEY"]])) {
      rlang::abort("config_error", message = missing_key_message(env_name, "DIDOSCALIM_API_KEY"))
    }
    if (is.null(config[[env_name]][["BASE_PATH"]])) {
      rlang::abort("config_error", message = missing_key_message(env_name, "DIDOSCALIM_BASE_PATH"))
    }
  }
  assign("environments", config, envir = .didoscalim_env)
}

#' Liste les environnements configurés
#'
#' Liste les environnements configurés avec les URL associées.
#'
#' @export
#'
#' @examples
#' list_envs()
list_envs <- function() {
  envs <- get("environments", envir = .didoscalim_env)
  message <- c(glue::glue("Vous avez {length(envs)} environnement(s) configuré(s) : "))
  for (e in names(envs)) {
    message <- c(message, i = glue::glue("{e} : {envs[[e]][['BASE_PATH']]}"))
  }
  cat(format_error_bullets(message))
}

#' @noRd
list_env_names <- function() {
  envs <- get("environments", envir = .didoscalim_env)
  names(envs)
}

#' Fixe l'environnement DiDo à utiliser
#'
#' @param env_name le nom de l'environnement à utiliser parmi PROD, ECOLE,
#'   PREPROD, DEV et suivant ce que vous avez configuré. Si env_name n'est pas
#'   passé, `set_didoscalim_work_env()` choisira le premier environnement configuré parmi
#'   DEV, PREPROD et ECOLE mais **jamais** PROD.
#' @param quiet quand TRUE ou que l'option dido_quiet est à TRUE supprime les
#'   messages d'information, `FALSE` par défaut
#'
#' @export
#'
#' @examples
#' set_work_env("DEV")
#'
#' set_work_env("PROD")
#'
#' set_work_env()
set_work_env <- function(env_name = NULL, quiet = NULL, .envir = parent.frame()) {
  environments <- get("environments", envir = .didoscalim_env)
  old_env <- get_work_env()

  if (exists("quiet", .envir) && !is.null(.envir$quiet)) {
    print(.envir$quiet)
    with_didoscalim_verbosity(
      "debug",
      didoscalim_debug(c(
        "!" = "Argument {.val quiet} is deprecated in favor of \\
                   {.val gargle_verbosity}",
        "i" = "Instead of: {.code function(..., quiet = TRUE)}",
        " " = 'Now do: {.code options(didoscalim_verbosity = "debug")}'))
    )
  }


  if (length(environments) == 0) {
    rlang::abort("env_error", message = "Aucun environnement n'est configuré dans votre Renviron")
  }

  if (is.null(env_name)) {
    env_name <- find_lowest_env()
    if (is.null(env_name)) {
      message <- c(
        glue::glue("Impossible de trouver l'environnement."),
        i = "Avez-vous configuré les environnements dans .Renviron ?"
      )
      rlang::abort("env_error", message = message)
    }
  }

  if (is.null(environments[[env_name]])) {
    message <- c(
      glue::glue("`{env_name}` n'est pas un environnement reconnu"),
      i = glue::glue("les environnements configurés dans Renviron sont {paste0(list_env_names(), collapse=', ')}")
    )
    rlang::abort("env_error", message = message)
  }
  if (!env_name %in% names(environments)) {
    rlang::abort("env_error", message = glue::glue("L'environnement {env_name} n'existe pas."))
  }

  didoscalim_info(c(x = glue::glue("Environnement DiDo actif : {env_name}")))
  options(list(didoscalim_work_env = env_name))
  return(invisible(old_env))
}

#' Récupère l'environnement utilisé
#'
#' Si aucun environnement n'a été fixé par `set_work_env()`, `get_work_env()`
#' retournera le premier environnement configuré en suivant l'ordre : DEV,
#' PREPROD, ECOLE. Il ne fixera **jamais** PROD implicitement.
#'
#' @inheritParams set_work_env
#'
#' @return une chaine de caractère avec l'environnement utilisé. Exemple "ECOLE"
#' @export
#'
#' @examples
#' get_work_env()
get_work_env <- function(quiet = NULL) {
  getOption("didoscalim_work_env", find_lowest_env())
}

#' @noRd
api_key <- function() {
  environments <- get("environments", envir = .didoscalim_env)
  environments[[get_work_env()]][["API_KEY"]]
}

#' @noRd
base_path <- function() {
  environments <- get("environments", envir = .didoscalim_env)
  environments[[get_work_env()]][["BASE_PATH"]]
}

find_lowest_env <- function() {
  environments <- get("environments", envir = .didoscalim_env)
  for (n in ordered_env_names) {
    if (!is.null(environments[[n]])) {
      return(n)
    }
  }
}

#' Vérifier l'accès aux différents environnements
#'
#' Affiche un état des connexions aux différents environnements avec le message
#' d'erreur le cas échéant
#'
#' @export
#'
#' @examples
#' check_envs()
check_envs <- function() {
  old_env <- get_work_env()
  message <- c("Test de connexion:")
  for (e in list_env_names()) {
    set_work_env(e)
    tryCatch(
      {
        me()
        message <- c(message, i = glue::glue("{e}: OK"))
      },
      error = function(error) {
        message <<- c(message, x = glue::glue("{e}: KO: {stringr::str_replace_all(error$message, '\n', ' ')}"))
      }
    )
  }
  #cat(format_error_bullets(message))
  didoscalim_info(format_error_bullets(message))
  set_work_env(old_env)
}

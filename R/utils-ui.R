# "Inspired" (as in mainly copied) from https://github.com/r-lib/gargle
# Copyright (c) 2020 RStudio; Google Inc
# See https://github.com/r-lib/gargle/blob/main/LICENSE.md

is_string <- function(input) {
  is.character(input) & length(input) == 1
}

#' @rdname didoscalim_options
#' @export
#' @section `didoscalim_verbosity`:
#' `didoscalim_verbosity()` returns the option named "didoscalim_verbosity", which
#' determines gargle's verbosity. There are three possible values, inspired by
#' the logging levels of log4j:
#' * "debug": Fine-grained information helpful when debugging, e.g. figuring out
#'   how `token_fetch()` is working through the registry of credential
#'   functions.
#' * "info" (default): High-level information that a typical user needs to see.
#'   Since typical gargle usage is always indirect, i.e. gargle is called by
#'   another package, gargle itself is very quiet. There are very few messages
#'   emitted when `didoscalim_verbosity = "info"`.
#' * "silent": No messages at all. However, warnings or errors are still thrown
#'   normally.
didoscalim_verbosity <- function() {
  gv <- getOption("didoscalim_verbosity", "info")

  vals <- c("debug", "info", "silent")
  if (!is_string(gv) || !(gv %in% vals)) {
    # ideally this would collapse with 'or' not 'and' but I'm going with it
    didoscalim_abort('Option "didoscalim_verbosity" must be one of: {.field {vals}}')
  }
  gv
}

#' @rdname didoscalim_options
#' @export
#' @param level Verbosity level: "debug" > "info" > "silent"
#' @param env The environment to use for scoping
local_didoscalim_verbosity <- function(level, env = parent.frame()) {
  withr::local_options(list(didoscalim_verbosity = level), .local_envir = env)
}

#' @rdname didoscalim_options
#' @export
#' @param code Code to execute with specified verbosity level
with_didoscalim_verbosity <- function(level, code) {
  withr::with_options(list(didoscalim_verbosity = level), code = code)
}

didoscalim_debug <- function(text, .envir = parent.frame()) {
  if (didoscalim_verbosity() == "debug") {
    cli::cli_bullets(text, .envir = .envir)
  }
}

didoscalim_info <- function(text, .envir = parent.frame()) {
  if (didoscalim_verbosity() %in% c("debug", "info")) {
    rlang::inform(message = text)
  }
}

# inspired by
# https://github.com/rundel/ghclass/blob/6ed836c0e3750b4bfd1386c21b28b91fd7e24b4a/R/util_cli.R#L1-L7
# more discussion at
# https://github.com/r-lib/cli/issues/222
cli_this = function(..., .envir = parent.frame()) {
  txt <- cli::cli_format_method(cli::cli_text(..., .envir = .envir))
  # @rundel does this to undo wrapping done by cli_format_method()
  # I haven't had this need yet
  # paste(txt, collapse = " ")
  txt
}

#' Error conditions for the gargle package
#'
#' @param class Use only if you want to subclass beyond `didoscalim_error`
#'
#' @keywords internal
#' @name gargle-conditions
#' @noRd
NULL

abort_if_not_one_line <- function(name,
                                message = NULL,
                                .envir = parent.frame(),
                                call = caller_env()) {
  data = rlang::env_get(.envir, name)

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


didoscalim_abort <- function(message, ...,
                         class = NULL,
                         .envir = parent.frame(),
                         call = caller_env()) {
  cli::cli_abort(
    message,
    class = c(class, "didoscalim_error"),
    .envir = .envir,
    call = call,
    ...
  )
}

didoscalim_abort_bad_class <- function(object,
                                   expected_class,
                                   call = caller_env()) {
  nm <- as_name(ensym(object))
  actual_class <- class(object)
  expected <- glue_collapse(
    didoscalim_map_cli(expected_class, template = "{.cls <<x>>}"),
    sep = ", ", last = " or "
  )
  msg <- glue("
    {.arg {nm}} must be <<expected>>, not of class {.cls {actual_class}}.",
              .open = "<<", .close =">>")
  didoscalim_abort(
    msg,
    class = "didoscalim_error_bad_class",
    call = call,
    object_name = nm,
    actual_class = actual_class,
    expected_class = expected_class
  )
}

didoscalim_abort_bad_params <- function(names,
                                    reason,
                                    endpoint_id,
                                    call = caller_env()) {
  didoscalim_abort(
    c(
      "These parameters are {reason}:",
      bulletize(didoscalim_map_cli(names), bullet = "x"),
      "i" = didoscalim_map_cli(
        endpoint_id,
        template = "API endpoint: {.field <<x>>}"
      )
    ),
    class = "didoscalim_error_bad_params",
    call = call,
    names = names,
    reason = reason
  )
}

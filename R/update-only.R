update_only <- function() {
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

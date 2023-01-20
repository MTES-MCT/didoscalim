compare_title <- function(column, string) {
  comparison_fun <- getOption("didoscalim_title_comparison", `==`)

  if (!is.function(comparison_fun)) {
    didoscalim_abort(glue::glue("l'option `didoscalim_title_comparison` doit être une fonction: \"{comparison_fun}\""), class = "bad_argument")
  }
  comparison_fun(clean_string(column), clean_string(string))
}

clean_string <- function(strings) {
  strings %>%
    tolower() %>%
    stringr::str_replace_all("[^[:alnum:]]", "")
}

MIN_SIZE_FOR_PROGRESS_BAR <- 10 * 1024 * 1024

#' Verse un fichier de données à intégrer
#'
#' @param file_name le nom du fichier
#'
#' @return un token
#' @export
#'
#' @examples
#' token <- dido_upload_file(dido_example("attachment.txt"))
#' @keywords internal
dido_upload_file <- function(file_name) {
  check_mandatory_arguments("file_name")

  if (file.exists(file_name) == FALSE) {
    rlang::abort("no_such_file", message = glue::glue("Le fichier `{file_name} n'existe pas"))
  }

  url <- "/files"
  headers <- c(
    "content-type" = "application/octet-stream",
    "x-uploadedfile-name" = basename(file_name)
  )
  file <- httr::upload_file(
    file_name,
    type = "application/octet-stream"
  )

  params <- list(
    method = "POST",
    path = url,
    body = ("file" <- file),
    headers = headers
  )
  if (file.info(file_name)$size > MIN_SIZE_FOR_PROGRESS_BAR) params <- c(params, list(progress = TRUE))

  result <- do.call(dido_api, params)
  return(result$tokenFile)
}

#' @noRd
check_csv <- function(token_file) {
  url <- glue::glue("/files/{token_file}/checkcsv")
  result <- dido_api(method = "GET", path = url)

  if (result$result == "invalid") {
    message <- format_check_csv_errors(result$message, result$errors)
    rlang::abort("invalid_file", message = message)
  } else if (result$result == "warning") {
    message <- format_check_csv_errors(result$message, result$errors)
    didoscalim_info(message)
  }

  invisible(TRUE)
}

format_check_csv_errors <- function(message, errors) {
  message <- c(x = message)
  for (col in names(errors)) {
    for (error in errors[[col]]) {
      msg <- stringr::str_replace(error$message, ", les valeurs.*", ".")

      message_pos <- length(message) + 1

      message[message_pos] <- glue::glue("{col}: {msg}")
      names(message)[message_pos] <- "x"
    }
  }
  return(message)
}

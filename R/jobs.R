#' Récupère tous les jobs liés à un utilisateur
#'
#' @return un tibble de jobs
#' @export
#'
#' @examples
#' jobs <- list_jobs()
list_jobs <- function() {
  url <- "/jobs"
  dido_api(method = "GET", path = url, as_tibble = TRUE)
}

#' Récupére un job en particulier
#'
#' @param data l'id du job
#'
#' @return un objet `dido_job()`
#' @export
#'
#' @examples
#' job <- list_jobs()[1, ]
#' get_job(job)
get_job <- function(data) {
  id <- get_job_id(data)
  url <- glue::glue("/jobs/{id}")
  job <- dido_api(method = "GET", path = url)
  dido_job(job)
}

#' Attend la fin d'un job d'intégration
#'
#' @param data l'id du job
#'
#' @return les données du job
#' @export
#'
#' @examples
#' job <- list_jobs()[1, ] %>%
#'   wait_for_job()
#' @keywords internal
wait_for_job <- function(data) {
  pb <- progress::progress_bar$new(
    total = 100,
    format = "chargement [:bar] :percent eta: :eta",
  )
  pb$tick(0)

  repeat {
    job <- get_job(data)
    if (!is.null(job$result)) {
      pb$terminate()
      return(dido_job(job))
    } else if (!is.null(job$error)) {
      pb$terminate()
      message <- c(glue::glue("   erreur {job$error$message}"))
      for (error in job$error$list) {
        message <- c(
          message,
          x = glue::glue("      ligne: {error$line} colonne: {error$column} {error$message}")
        )
      }

      rlang::abort("datafile_error", message = message)
    }

    if (job$state$status != "failed" && !is.null(job$state$progress$percentage)) {
      ratio <- as.integer(as.double(job$state$progress$percentage)) / 100
      if (!pb$finished) pb$update(ratio)
    }
    Sys.sleep(5)
  }
}

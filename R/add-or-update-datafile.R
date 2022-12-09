#' Ajoute ou modifie un datafile
#'
#' @description
#' met à jour le datafile avec le même titre s'il existe sinon ajoute un datafile
#'
#' @inheritParams add_datafile
#' @param on_existing_millesime skip/fail/replace : action à faire quand le millésime existe déjà. Peut-être :
#'   * "skip" : retourne immédiatement après avoir affiché un message
#'   * "fail" : lève une exception de class `existing_millesime`
#'   * "replace" : remplace le millésime du même identifiant
#' @param keep_old_millesimes nombre d'ancien millésimes à conserver, les autres sont supprimés.
#' @param check_file_date TRUE/FALSE, Si TRUE met à jour le datafile uniquement
#'   si le fichier est plus récent que le last_modified du datafile
#'
#' @return un objet `dido_job()` ou `NULL` si aucune création/mise à jour
#'   n'a eu lieu
#'
#' @family datafile
#'
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dataset <- add_or_update_dataset(
#'   title = "Un dataset pour les add_or_update_datafiles",
#'   description = "Description des données statistiques",
#'   topic = "Transports",
#'   frequency = "unknown",
#' )
#'
#' add_or_update_datafile(
#'   dataset = dataset,
#'   title = "titre",
#'   description = "description",
#'   file_name = dido_example("augmente.csv"),
#'   millesime = "2021-10",
#' )
#'
#' add_or_update_datafile(
#'   dataset = dataset,
#'   title = "titre",
#'   description = "description",
#'   file_name = dido_example("augmente.csv"),
#'   temporal_coverage_start = "2021-01-01",
#'   temporal_coverage_end = "2021-12-31",
#'   millesime = "2022-10",
#' )
add_or_update_datafile <- function(dataset,
                                   title,
                                   description,
                                   file_name,
                                   millesime = format(Sys.time(), "%Y-%m"),
                                   published = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                   temporal_coverage_start = NULL,
                                   temporal_coverage_end = NULL,
                                   legal_notice = "SDES",
                                   date_diffusion = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                   keep_old_millesimes = Inf,
                                   on_existing_millesime = "fail",
                                   check_file_date = FALSE,
                                   quiet = NULL) {
  check_mandatory_arguments("dataset", "title", "description")
  rlang::arg_match0(on_existing_millesime, c("skip", "fail", "replace"))

  datafiles <- dataset %>%
    list_datafiles() %>%
    filter(.data$title == .env$title)

  if (nrow(datafiles) == 0) {
    job_result <- add_datafile(
      dataset = dataset,
      title = title,
      description = description,
      file_name = file_name,
      millesime = millesime,
      published = published,
      temporal_coverage_start = temporal_coverage_start,
      temporal_coverage_end = temporal_coverage_end,
      legal_notice = legal_notice,
      date_diffusion = date_diffusion
    )
    return(invisible(job_result))
  }

  abort_if_not_one_line("datafiles", message = c(x = glue::glue("Il y a plusieurs datafiles avec le titre `{title}`.")))

  if (nrow(datafiles) == 1) {
    datafile <- get_datafile(datafiles[1, ])

    if (check_file_date) {
      datafile_last_modified <- lubridate::as_datetime(datafile$last_modified)
      file_mtime <- lubridate::as_datetime(file.info(file_name)$mtime)
      if (file_mtime < datafile_last_modified) {
        return(NULL)
      }
    }

    existing_millesimes <- datafile %>%
      list_millesimes() %>%
      filter(.data$millesime != .env$millesime)

    job_result <- NULL
    try_fetch(
      {
        # add the millesime
        job_result <- add_millesime(
          datafile = datafile,
          file_name = file_name,
          millesime = millesime,
          date_diffusion = date_diffusion
        )
      },
      millesime_exists = function(cnd) {
        msg <- (c("Le millesime existe déjà :",
          i = glue::glue("dataset: {dataset$title}"),
          i = glue::glue("datafile: {datafile$title}"),
          i = glue::glue("millesime: {millesime}")
        ))

        if (on_existing_millesime == "skip") {
          with_didoscalim_verbosity("info", {
            didoscalim_info(msg)
          })
          return(NULL)
        } else if (on_existing_millesime == "replace") {
            job_result <<- replace_millesime(
              datafile = datafile,
              file_name = file_name,
              millesime = millesime,
              date_diffusion = date_diffusion
            )
        } else if (on_existing_millesime == "fail") {
          didoscalim_abort(cnd$message, class = class(cnd), call = caller_env())
        }
      }
    )
    if (is.null(job_result)) {
      return(NULL)
    }

    datafile <- get_datafile(datafiles[1, ])
    origin <- duplicate(datafile)

    # update the datafile
    if (!missing(description)) datafile$description <- description
    if (!missing(published)) datafile$published <- published
    if (!missing(legal_notice)) datafile$legal_notice <- legal_notice
    if (!missing(date_diffusion)) datafile$date_diffusion <- date_diffusion
    if (!missing(temporal_coverage_start)) datafile$temporal_coverage$start <- temporal_coverage_start
    if (!missing(temporal_coverage_end)) datafile$temporal_coverage$end <- temporal_coverage_end

    if (!identical(origin, datafile)) {
      update_datafile(datafile)
    }

    # removed unwanted millesime
    if (!millesime %in% c("skip", "replace")) {
      millesimes_to_delete <- find_millesimes_to_delete(existing_millesimes, keep_old_millesimes)
      for (m in millesimes_to_delete$millesime) delete_millesime(datafile, m)
    }
    return(invisible(job_result))
  }
}


find_millesimes_to_delete <- function(existing_millesimes, keep_last_n) {
  existing_millesimes %>%
    dplyr::arrange(.data$millesime) %>%
    utils::head(nrow(existing_millesimes) - keep_last_n)
}

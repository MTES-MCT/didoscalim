#' Ajoute ou modifie un datafile
#'
#' @description
#' met à jour le datafile (métadonnées et données) avec le même titre s'il
#' existe sinon ajoute un datafile, un millésime ou remplace un millésime s'il
#' existe déjà.
#'
#' Le paramètre `on_existing_millesime` permet de préciser le comportement à
#' adopter si le millésime existe déjà.
#'
#' Si plusieurs datafiles ont le même titre, cette méthode retourne une erreur.
#'
#' @inheritParams add_datafile
#' @param on_existing_millesime skip/fail/replace : action à faire quand le
#'   millésime existe déjà. Peut-être :
#'   * "skip" : retourne immédiatement après avoir affiché un message. Les métadonnées du millésime ne sont pas mises à jour et les anciens millésimes ne sont jamais effacés.
#'   * "fail" : lève une exception de class `existing_millesime`. Les métadonnées du millésime ne sont pas mises à jour et les anciens millésimes ne sont jamais effacés.
#'   * "replace" : remplace le millésime ayant le même identifiant
#' @param keep_old_millesimes nombre d'ancien millésimes à conserver. Ce
#'   paramètre permet de supprimer les anciens millésimes. Pour des raisons de
#'   sécurité, cette option n'effacera jamais des millésimes plus récent que le
#'   millésime passé en paramètre.
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
    filter(compare_title(.data$title, .env$title))

  if (nrow(datafiles) == 0) {

    if (update_only()) {
      abort_update_only(title, "datafile")
    }

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

    job_result <- NULL

    millesime_already_exists <- millesime %in% list_millesimes(datafile)$millesime
    if (!millesime_already_exists) {
      # add the millesime
      job_result <- add_millesime(
        datafile = datafile,
        file_name = file_name,
        millesime = millesime,
        date_diffusion = date_diffusion
      )
    } else {
      msg <- (c("Le millesime existe déjà :",
                i = glue::glue('dataset: "{dataset$title}"'),
                i = glue::glue('datafile: "{datafile$title}"'),
                i = glue::glue('millesime: "{millesime}"')
      ))
      if (on_existing_millesime == "skip") {
        with_didoscalim_verbosity("info", {
          didoscalim_info(msg)
        })
        return(NULL)
      } else if (on_existing_millesime == "replace") {
        job_result <- replace_millesime(
          datafile = datafile,
          file_name = file_name,
          millesime = millesime,
          date_diffusion = date_diffusion
        )
        didoscalim_info(glue::glue('datafile "{datafile$title}": remplacement du millesime {millesime}'))
      } else if (on_existing_millesime == "fail") {
        didoscalim_abort(msg, class = "millesime_exists", call = caller_env())
      }
    }
    if (is.null(job_result)) {
      return(NULL)
    }

    datafile <- get_datafile(datafiles[1, ])
    origin <- duplicate(datafile)

    # update the datafile
    datafile$title <- toString(title)
    if (!missing(description)) datafile$description <- toString(description)
    if (!missing(published)) datafile$published <- published
    if (!missing(legal_notice)) datafile$legal_notice <- toString(legal_notice)
    if (!missing(date_diffusion)) datafile$date_diffusion <- date_diffusion
    if (!missing(temporal_coverage_start)) datafile$temporal_coverage$start <- temporal_coverage_start
    if (!missing(temporal_coverage_end)) datafile$temporal_coverage$end <- temporal_coverage_end

    if (!identical(origin, datafile)) {
      update_datafile(datafile)
    }

    # removed unwanted millesime FIXME
    if (!on_existing_millesime %in% c("skip")) {
      millesimes_to_delete <- find_millesimes_to_delete(list_millesimes(datafile),
                                                        keep_old_millesimes,
                                                        millesime)
      for (m in millesimes_to_delete$millesime) delete_millesime(datafile, m)
    }
    return(invisible(job_result))
  }
}

# take a tibble of datafile millesimes and return the (nb of millesimes - keep_old_millesimes) older than millesime.
find_millesimes_to_delete <- function(existing_millesimes, keep_old_millesimes, millesime) {
  older_millesimes <- existing_millesimes %>%
    dplyr::filter(.data$millesime < .env$millesime)

  older_millesimes %>%
    dplyr::arrange(.data$millesime) %>%
    utils::head(nrow(older_millesimes) - keep_old_millesimes)
}

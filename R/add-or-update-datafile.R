#' Ajoute un datafile à un dataset
#'
#' @inheritParams add_datafile
#' @param keep_old_millesimes

#'
#' @return un objet `dido_job()`
#'
#' @family datafile
#'
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
#'
#' dataset <- list_datasets() %>%
#'   filter(title == "Données de consommation fictive")
#'
#' add_datafile(
#'   dataset = dataset,
#'   title = "titre",
#'   description = "description",
#'   file_name = dido_example("augmente.csv")
#' )
#'
#' # publier un fichier de données avec toutes les métadonnées et un embargo
#' add_datafile(
#'   dataset = dataset,
#'   title = "titre 2",
#'   description = "description 2",
#'   file_name = dido_example("augmente.csv"),
#'   temporal_coverage_start = "2021-01-01",
#'   temporal_coverage_end = "2021-12-31",
#'   legal_notice = "something",
#'   millesime = "2020-10",
#'   date_diffusion = "2020-11-01 08:00:00"
#' )
add_or_update_datafile <- function(dataset,
                                   title,
                                   description,
                                   file_name,
                                   millesime = format(Sys.time(), "%Y-%m"),
                                   published = format(Sys.time(), "%Y-%m-%d"),
                                   temporal_coverage_start = NULL,
                                   temporal_coverage_end = NULL,
                                   legal_notice = "SDES",
                                   date_diffusion = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                   keep_old_millesimes = Inf,
                                   quiet = NULL) {

  datafiles <- dataset %>% list_datafiles()

  if (nrow(datafiles) > 0) {
    df <- datafiles %>% filter(.data$title == .env$title)
  }

  if (nrow(datafiles) == 0 || nrow(df) == 0) {
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

  if (nrow(df) > 1) {
    abort_not_one_ligne(df)
  }

  if (nrow(df) == 1) {
    datafile = get_datafile(df)

    millesimes <- datafile %>% list_millesimes()

    # add the millesime
    job_result <- add_millesime(
       datafile = datafile,
       file_name = dido_example("augmente.csv"),
       millesime = millesime,
       date_diffusion = date_diffusion
    )
    # update the datafile
    if (!missing(description)) datafile$description = description
    if (!missing(published)) datafile$published = published
    if (!missing(legal_notice)) datafile$legal_notice = legal_notice
    if (!missing(date_diffusion)) datafile$date_diffusion = date_diffusion
    if (!missing(temporal_coverage_start)) datafile$temporal_coverage$start = temporal_coverage_start
    if (!missing(temporal_coverage_end)) datafile$temporal_coverage$end = temporal_coverage_end
    update_datafile(datafile)

    # removed unwanted millesime
    millesimes_to_delete <- find_millesimes_to_delete(millesimes, keep_old_millesimes)
    for (m in millesimes_to_delete$millesime) delete_millesime(datafile, m)

    return(invisible(job_result))
  }
}


find_millesimes_to_delete <- function(millesimes, keep_last_n) {
  millesimes %>% dplyr::arrange(millesime) %>% utils::head(nrow(millesimes)-keep_last_n)
}

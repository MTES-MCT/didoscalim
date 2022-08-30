#' Prépare un objet pour l'envoyer à DiDo
#'
#' Les objets `dido_dataset()` et `dido_datafile()` retournés par
#' `get_dataset()`, `get_datafile()`, ... portent des informations sur les
#' objets enfants (fichiers annexes, datafiles, millésimes...). Cette fonction
#' permet de les supprimer pour ne garder que les métadonnées propres à l'objet.
#'
#' @param data objet dido_dataset, dido_datafile
#'
#' @return un objet du même type avec uniquement ses métadonnées propres.
#' @export
#'
#' @examples
#' dataset <- get_dataset(list_datasets()[1, ])
#' clean_metadata(dataset)
#'
#' datafile <- get_datafile(list_datafiles()[1, ])
#' clean_metadata(datafile)
clean_metadata <- function(data) UseMethod("clean_metadata")

#' @noRd
internal_clean_metadata <- function(data) UseMethod("internal_clean_metadata")

#' Retourne le dataset id de l'objet
#'
#' @param data un objet dido_dataset, dido_datafile ou dido_job
#'
#' @return le dataset id
#' @export
#'
#' @examples
#' ds <- list_datasets()[1, ]
#' get_dataset_id(ds)
#' @keywords internal
get_dataset_id <- function(dataset) UseMethod("get_dataset_id")

#' @export
get_dataset_id.default <- function(dataset) NULL

#' @export
get_dataset_id.character <- function(dataset) {
  if (is_mongo_oid(dataset)) {
    return(dataset)
  }
  NULL
}

#' @export
get_dataset_id.data.frame <- function(dataset) {
  if (!"id" %in% names(dataset)) abort_not_dataset()
  abort_if_not_one_line("dataset")
  if (is_mongo_oid(dataset[["id"]])) return(dataset[["id"]])
  NULL
}

#' Retourne le datafile rid de l'objet
#'
#' @param data un objet dido_datafile ou dido_job
#'
#' @return le datafile id
#' @export
#'
#' @examples
#' df <- list_datafiles()[1, ]
#' get_datafile_rid(df)
#' @keywords internal
get_datafile_rid <- function(datafile) UseMethod("get_datafile_rid")

#' @export
get_datafile_rid.default <- function(datafile) NULL

#' @export
get_datafile_rid.character <- function(datafile) {
  if (is_uuid(datafile)) {
    return(datafile)
  }
  NULL
}

#' @export
get_datafile_rid.data.frame <- function(datafile) {
  if (!"rid" %in% names(datafile)) abort_not_datafile()
  abort_if_not_one_line("datafile")
  if (is_uuid(datafile[["rid"]])) return(datafile[["rid"]])
  NULL
}

#' Retourne l'attachment id de l'objet
#'
#' @param data un objet dido_attachment ou une chaine
#'
#' @return le rid de l'attachment
#' @export
#'
#' @examples
#' at <- list_attachments()[1, ]
#' get_attachment_rid(at)
#' @keywords internal
get_attachment_rid <- function(attachment) UseMethod("get_attachment_rid")

#' @export
get_attachment_rid.default <- function(attachment) NULL

#' @export
get_attachment_rid.character <- function(attachment) {
  if (is_uuid(attachment)) {
    return(attachment)
  }
  NULL
}

#' @export
get_attachment_rid.data.frame <- function(attachment) {
  if (!"rid" %in% names(attachment)) abort_not_attachment()
  abort_if_not_one_line("attachment")
  if (is_uuid(attachment[["rid"]])) return(attachment[["rid"]])
  NULL
}

#' Retourne l'attachment id de l'objet
#'
#' @param data un objet dido_attachment ou une chaine
#'
#' @return le rid de l'attachment
#' @export
#'
#' @examples
#' j <- list_jobs()[1, ]
#' get_job_id(j)
#' @keywords internal
get_job_id <- function(data) UseMethod("get_job_id")

#' @export
get_job_id.default <- function(data) NULL

#' @export
get_job_id.character <- function(data) data

#' @export
get_job_id.integer <- function(data) data

#' @export
get_job_id.data.frame <- function(job_id) {
  if (!"id" %in% names(job_id)) abort_not_job()
  abort_if_not_one_line("job_id")
  return(job_id[["id"]])
}

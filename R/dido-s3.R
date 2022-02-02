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
get_dataset_id <- function(data) UseMethod("get_dataset_id")

#' @export
get_dataset_id.default <- function(data) NULL

#' @export
get_dataset_id.character <- function(data) {
  if (is_mongo_oid(data)) {
    return(data)
  }
  NULL
}

#' @export
get_dataset_id.data.frame <- function(data) {
  if (!"id" %in% names(data)) abort_not_dataset()
  if (nrow(data) != 1) abort_not_one_ligne(data)
  return(data[["id"]])
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
get_datafile_rid <- function(data) UseMethod("get_datafile_rid")

#' @export
get_datafile_rid.default <- function(data) NULL

#' @export
get_datafile_rid.character <- function(data) {
  if (is_uuid(data)) {
    return(data)
  }
  NULL
}

#' @export
get_datafile_rid.data.frame <- function(data) {
  if (!"rid" %in% names(data)) abort_not_datafile()
  if (nrow(data) != 1) abort_not_one_ligne(data)
  return(data[["rid"]])
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
get_attachment_rid <- function(data) UseMethod("get_attachment_rid")

#' @export
get_attachment_rid.default <- function(data) NULL

#' @export
get_attachment_rid.character <- function(data) {
  if (is_uuid(data)) {
    return(data)
  }
  NULL
}

#' @export
get_attachment_rid.data.frame <- function(data) {
  if (!"rid" %in% names(data)) abort_not_attachment()
  if (nrow(data) != 1) abort_not_one_ligne(data)
  return(data[["rid"]])
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
get_job_id.data.frame <- function(data) {
  if (!"id" %in% names(data)) abort_not_job()
  if (nrow(data) != 1) abort_not_one_ligne(data)
  return(data[["id"]])
}

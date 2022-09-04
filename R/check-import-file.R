#' Vérifie un fichier CSV
#'
#' @description
#' Cette fonction permet de vérifier rapidement la validité d'un fichier de
#' données. Elle :
#'
#' 1. créé un jeu de données avec un nom aléatoire
#' 2. ajoute le fichier de données
#' 3. supprime le jeu de données
#' 4. affiche les erreurs rencontrées s'il y en a
#'
#' **L'import de test ne se fera jamais sur l'environnement de production** mais
#' sur un des environnments DEV, PREPROD ou ECOLE. Si aucun de ces
#' environnements n'est configuré, la fonction remonte une erreur
#'
#' @param file_name le fichier a valider
#'
#' @return TRUE si l'import s'est bien passé, FALSE sinon
#' @export
#'
#' @examples
#'
#' check_import_file(dido_example("file-with-error.csv"))
check_import_file <- function(file_name) {
  local_didoscalim_verbosity("silent")
  withr::local_options(didoscalim_work_env = find_lowest_env())

  dataset <- add_dataset(
    title = glue::glue("check_import_file {runif(1)}"),
    description = "un message",
    topic = "Transports",
    frequency = "unknown",
  )

  tryCatch({
      add_datafile(dataset,
                   title = "titre 2",
                   description = "description 2",
                   file_name = file_name)
    local_didoscalim_verbosity("info")
    didoscalim_info(c(
        "!" = glue::glue("Pas d'erreur dans le fichier `{file_name}`")
      ))
      return(TRUE)
    },
    error = function(error) {
      local_didoscalim_verbosity("info")
      didoscalim_info(c(
        "x" = glue::glue("Problème sur le fichier `{file_name}`"),
        error$message))
      return(FALSE)
    },
    finally = delete_dataset(dataset)
  )

}

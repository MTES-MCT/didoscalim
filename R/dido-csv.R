default_columns <- list(
  REGION = list(type = "cog_region_{COG_YEAR}", description = "Code de la région"),
  DEPARTEMENT = list(type = "cog_departement_{COG_YEAR}", description = "Code du département"),
  COMMUNE = list(type = "cog_commune_{COG_YEAR}", description = "Code de la commune"),
  EPCI = list(type = "cog_epci_{COG_YEAR}", description = "Code de l'EPCI"),
  IRIS = list(type = "cog_iris_{COG_YEAR}", description = "Code de l'iris"),
  ANNEE = list(type = "annee", description = "Millésime des données"),
  MOIS = list(type = "mois", description = "Mois des données")
)

#' Génère les lignes d'entête du CSV augmenté
#'
#' Génère un dataframe avec les lignes d'entêtes du CSV augmenté comme premières
#' lignes.
#'
#' Cette fonction essaye de deviner le type des colonnes pour leur donner un
#' type DiDo de base :
#'
#' | type R          | type en sortie |
#' |-----------------|----------------|
#' | double          | nombre         |
#' | integer         | entier         |
#' | date            | jour           |
#' | logical         | booleen        |
#' | tous les autres | texte          |
#'
#'
#' @param data le dataframe à augmenter
#' @param params une liste nommée décrivant les caractéristiques des colonnes :
#'   ```{r, results = FALSE}
#'   list(
#'     COL1 = list(description = "une description"),
#'     COL2 = list(unit = "MWh")
#'   )
#'   ```
#'   Les caractéristiques disponibles sont :
#'
#'   * `description`: la description de la colonne
#'   * `type`: nombre, entier, texte, ...
#'   * `unit`: l'unité de la colonne
#'
#'   Le nom de la colonne peut être une expression rationnelle :
#'   ```{r, results = FALSE}
#'   list(
#'     COL     = list(description = "une description"),
#'     `COL.*` = list(unit = "MWh")
#'   )
#'   ```
#'   La première colonne correspondante est utilisée, **mettez toujours vos
#'   expressions rationnelles à la fin**.
#'
#'   La valeur du champ `description` est évaluée par glue::glue avec un
#'   paramètre `name` qui correspond au nom de la variable du dataframe
#'   d'origine.
#'
#'   ```{r, results = FALSE}
#'   list(
#'     COL     = list(description = "une description"),
#'     `COL.*` = list(
#'       unit = "MWh",
#'       description = "une description {string::str_extract(name, '\\d{4}')}"
#'     )
#'   )
#'   ````
#'
#' @param locale la locale à utiliser. Le seul élément à configurer à ce niveau
#'   est le séparateur décimal. Par défaut c'est le point (`.`). Pour
#'   sélectionner la virgule vous pouvez utiliser :
#'   ```{r}
#'   locale = locale(decimal_mark = ",")
#'   ```
#' @param cog_year le millésime du COG utilisé si besoin. Par défaut prend
#'   l'année en cours
#'
#' @return un dataframe avec les 4 lignes de description du csv augmenté
#' @export
#'
#' @details Certains noms de variable sont connus par didoscalim qui génère
#'   automatiquement le type et la description. La liste complète de ces
#'   variables et des types/descriptions associés est :
#'
#' | nom de la variable | type                  | description             |
#' |--------------------|-----------------------|-------------------------|
#' | REGION             |cog_region_AAAA        | Code de la région       |
#' | DEPARTEMENT        |cog_departement_AAAA   | Code du département     |
#' | COMMUNE            |cog_commune_AAAA       | Code de la commune      |
#' | EPCI               |cog_epci_AAAA          | Code de l'EPCI          |
#' | IRIS               |cog_iris_AAAA          | Code de l'IRIS          |
#' | ANNEE              | n/a                   | Millésimes des données  |
#' | MOIS               | n/a                   | mois des données        |
#'
#' L'année `AAAA` est par défaut l'année courante, vous pouvez la modifier en
#' passant le paramètre `cog_year`
#'
#' @seealso En complément, vous pouvez lire : [la description d'un fichier csv
#'   augmenté](https://cgdd.gitlab-pages.din.developpement-durable.gouv.fr/sdsed-bun/datalake/api/040-csvfile/),
#'    [la liste des entêtes
#'   utilisables](https://cgdd.gitlab-pages.din.developpement-durable.gouv.fr/sdsed-bun/datalake/api/210-headers/)
#'
#'
#' @examples
#' data <- data.frame(
#'   OPERATEUR = c("nom1", "nom2"),
#'   COMMUNE = c("29000", "35000"),
#'   CONSO = c(1, 2)
#' )
#' params <- list(
#'   OPERATEUR = list(description = "L'opérateur"),
#'   CONSO = list(description = "La consommation", unit = "Mwh")
#' )
#' dido_csv(data, params = params)
#'
#' data <- data.frame(
#'   DONNEES_2021 = c("1,4", "1,5"),
#'   DONNEES_2022 = c("1,3", "1,8")
#' )
#' params <- list(
#'   `DONNEES_.*` = list(description = 'description pour {stringr::str_extract(name, "\\\\d{4}")}')
#' )
#' dido_csv(data, params = params, locale = locale(decimal_mark = ","))
dido_csv <- function(data, params = list(),
                     locale = readr::default_locale(),
                     cog_year = format(Sys.time(), "%Y")) {
  data <- dplyr::mutate(data, dplyr::across(!where(is.character), as.character))
  desc <- description_row(data, params)
  type <- type_row(data, params, locale, cog_year)
  unit <- unit_row(type, params)
  name <- name_row(data)

  dplyr::bind_rows(desc, type, unit, name, data)
}

#' @noRd
description_row_glue <- function(description, name) {
  if (is.null(description)) return(NULL)
  stringr::str_glue(description)
}

#' @noRd
description_row <- function(data, params = list()) {
  name_cols <- vapply(names(data), function(name) {
    description_row_glue(matching_param(params, name)[["description"]], name) %||%
      default_columns[[name]][["description"]] %||%
      name
  }, character(1))
  return(name_cols)
}

#' default unit per type
#' @noRd
list_units <- list(
  nombre = "s/u",
  entier = "s/u"
)

#' @noRd
unit_row <- function(data_type, params = list()) {
  data_unit <- vapply(names(data_type), function(name) {
    matching_param(params, name)[["unit"]] %||%
      list_units[[data_type[[name]]]] %||%
      "n/a"
  }, character(1))
}

list_types <- list(
  double = "nombre",
  integer = "entier",
  logical = "booleen",
  date = "jour"
)

#' @noRd
guess_col <- function(column, locale) {
  list_types[[guess_parser(column,
    na = c("", "na", "s", "secret"),
    guess_integer = TRUE,
    locale = locale
  )]]
}

#' @noRd
matching_param <- function(params, name) {
  regex_names <- vapply(names(params), function(v) {
    glue::glue("^{v}$")
  }, character(1))
  matching_param <- params[str_detect(name, regex_names)]

  if (length(matching_param) == 0) {
    return(list())
  }
  matching_param[[1]]
}

#' @noRd
type_row <- function(data, params = list(), locale, cog_year) {
  cog_year <- toString(cog_year)

  guess_cols <- vapply(names(data), function(name) {
    col_type <- matching_param(params, name)[["type"]] %||%
      default_columns[[name]][["type"]] %||%
      guess_col(data[[name]], locale) %||%
      "texte"

    str_replace(col_type, "\\{COG_YEAR\\}", cog_year)
  }, character(1))
}

#' @noRd
name_row <- function(data) {
  name_cols <- vapply(names(data), function(name) {
    toupper(stringr::str_replace_all(name, " .*", "_"))
  }, character(1))
  return(name_cols)
}

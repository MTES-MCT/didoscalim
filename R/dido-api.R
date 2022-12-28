cache <- new.env(parent = emptyenv())
cache$default_ua <- NULL

default_ua <- function() {
  if (is.null(cache$default_ua)) {
    versions <- c(
      httr = as.character(utils::packageVersion("httr")),
      didoscalim = as.character(utils::packageVersion("didoscalim"))
    )
    cache$default_ua <- paste0(names(versions), "/", versions, collapse = " ")
  }
  cache$default_ua
}

#' Envoie une requête au serveur DiDo
#'
#' Envoie la requête au serveur et retourne le résultat sous forme d'un objet ou un dataframe si as_dataframe est à TRUE
#'
#' @param method une des méthodes GET/POST/PUT/DELETE
#' @param path le chemin de l'api
#' @param body le body de la requête
#' @param query_params les paramètres de la requête
#' @param headers les entêtes de la requête
#' @param as_tibble TRUE/FALSE si TRUE retourne un tibble à la place d'un objet.
#'   Défaut à FALSE
#'
#' @return un objet json ou un dataframe
#' @export
#'
#' @examples
#' alerts <- dido_api(method = "GET", path = "/datasets/alerts", as_tibble = TRUE)
#' @keywords internal
dido_api <- function(method, path, body = NULL, query_params = list(), headers = c(), as_tibble = FALSE, progress = FALSE) {
  if (!method %in% c("GET", "PUT", "POST", "DELETE")) {
    rlang::abort(glue::glue("unknown method: {method} for url: {url}"))
  }

  url <- paste0(base_path(), path)
  ua <- httr::user_agent(default_ua())

  headers["x-api-key"] <- api_key()
  if (!"content-type" %in% headers) headers["content-type"] <- "application/json"

  params <- list(method, url, c(httr::add_headers(headers), ua), terminate_on = c(400:499))
  #if (!missing(query_params)) params <- c(params, query = query_params)
  if (!missing(body)) params <- c(params, list(body = body))
  if (progress) params <- append(params, httr::progress(type = c("up"), con = stdout()))

  params <- list(
    method, url,
    c(httr::add_headers(headers), ua),
    terminate_on = c(400:499),
    body = body,
    if (progress && interactive()) httr::progress(type = c("up"), con = stdout())
  )

  #str(params)

  didoscalim_debug(glue::glue("{ str(params) }"))

  response <- do.call(httr::RETRY, params)

  if (httr::status_code(response) >= 400) {
    didoscalim_abort(
      extract_error(response),
      class = c(
        "api_error",
        glue::glue("http_error_{httr::status_code(response)}")
      )
    )
  }
  content <- httr::content(response, as = "text", encoding = "UTF-8")

  if (nzchar(content) == 0) {
    return(TRUE)
  }

  result <- jsonlite::fromJSON(content, simplifyVector = FALSE, simplifyDataFrame = as_tibble, flatten = as_tibble)
  if (as_tibble) {
    return(as_tibble(result))
  }
  result
}

#' @noRd
extract_error <- function(response) {
  if (httr::status_code(response) >= 400 && httr::status_code(response) < 500) {
    msg <- c(x = glue::glue("Client error : {httr::status_code(response)}"))
  } else if (httr::status_code(response) >= 500) {
    msg <- c(x = glue::glue("Server error : {httr::status_code(response)}"))
  }

  if (httr::http_type(response) == "application/json") {
    json_msg <- jsonlite::fromJSON(httr::content(response, "text"), simplifyVector = FALSE)
    msg <- c(x = glue::glue("{json_msg$message}: {json_msg$code}"))

    if (!is.null(json_msg$errors)) {
      if (inherits(json_msg$errors, "data.frame")) {
        errors <- tidyr::unnest(json_msg$errors, .data$messages) %>% tidyr::unite("errors", .data$field:.data$messages, sep = ": ")
        for (e in errors[["errors"]]) msg <- c(msg, x = e)
      } else if (inherits(json_msg$errors, "list")) {
        for (e in json_msg$errors) msg <- c(msg, x = e)
      } else {
        msg <- c(msg, x = json_msg$errors)
      }
    }
  }
  c(msg, i = glue::glue("url: {response$url}\n"))
}

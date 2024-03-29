#' @keywords internal
#' @aliases didoscalim-package
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom httr GET POST PUT DELETE status_code add_headers status_code user_agent
#' @importFrom readr read_delim write_delim guess_parser cols default_locale locale
#' @importFrom tidyr unnest unite
#' @importFrom tibble tibble as_tibble
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows select slice filter mutate across arrange
#' @importFrom stringr str_replace str_detect fixed str_replace_all str_extract
#' @importFrom magrittr %>%
#' @import rlang
#' @importFrom glue glue
#' @importFrom purrr pwalk
#' @importFrom utils str
#' @importFrom stats runif
#' @importFrom lubridate format_ISO8601 ymd_hms now time_length
#' @importFrom utils head
## usethis namespace: end
NULL

# See issue https://github.com/r-lib/tidyselect/issues/201
utils::globalVariables(c("where", ".env"))

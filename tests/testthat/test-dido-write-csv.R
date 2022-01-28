test_that("dido_read_delim works for default locale", {
  tbl <- dido_read_delim(paste0(test_path(), "/example-default.csv"))

  params <- list(
    DESCRIPTION = list(description = "Une description"),
    UNIT_MWH = list(unit = "MWh"),
    TYPE_NAF = list(type = "naf_division")
  )
  result <- dido_csv(tbl, params = params)

  expected <- read_delim(paste0(test_path(), "/example-default-result.csv"),
    col_types = readr::cols(.default = "c")
  )

  expect_equal(result, expected)
})

test_that("dido_read_delim works with ISO-8859-15", {
  locale <- readr::locale(encoding = "ISO-8859-15")

  tbl <- dido_read_delim(
    paste0(test_path(), "/example-iso-8859-15.csv"),
    locale = locale
  )

  params <- list(
    DESCRIPTION = list(description = "Une description"),
    UNIT_MWH = list(unit = "MWh"),
    TYPE_NAF = list(type = "naf_division")
  )
  result <- dido_csv(
    tbl,
    params = params,
    locale = locale
  )

  expected <- read_delim(
    paste0(test_path(), "/example-default-result.csv"),
    col_types = readr::cols(.default = "c")
  )

  expect_equal(result, expected)
})

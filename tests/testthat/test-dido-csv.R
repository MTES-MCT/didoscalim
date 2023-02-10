withr::local_options(warn=-1)

test_that("dido_csv works for default types", {
  data <- tibble::tibble(
    CHAR = c("a"),
    INTEGER = c(1),
    NUMBER = c(1.2)
  )
  expected <- tibble::tibble(
    CHAR = c("CHAR", "texte", "n/a", "CHAR", "a"),
    INTEGER = c("INTEGER", "entier", "s/u", "INTEGER", "1"),
    NUMBER = c("NUMBER", "nombre", "s/u", "NUMBER", "1.2"),
  )

  result <- dido_csv(data)

  expect_equal(result, expected)
})

test_that("dido_csv works with locale", {
  data <- tibble::tibble(
    CHAR = c("a"),
    INTEGER = c(1),
    NUMBER = c("1,2")
  )
  expected <- tibble::tibble(
    CHAR = c("CHAR", "texte", "n/a", "CHAR", "a"),
    INTEGER = c("INTEGER", "entier", "s/u", "INTEGER", "1"),
    NUMBER = c("NUMBER", "nombre", "s/u", "NUMBER", "1,2"),
  )

  result <- dido_csv(data, locale = readr::locale(decimal_mark = ","))

  expect_equal(result, expected)
})


test_that("dido_csv works for columns with default values", {
  data <- tibble::tibble(
    REGION = c("22"),
    DEPARTEMENT = c("22"),
    COMMUNE = c("22"),
    EPCI = c("22"),
    IRIS = c("22"),
    ANNEE = c("2022"),
    MOIS = c("2022-01")
  )
  expected <- tibble::tibble(
    REGION = c("Code de la région", glue::glue("cog_region_{format(Sys.time(), '%Y')}"), "n/a", "REGION", "22"),
    DEPARTEMENT = c("Code du département", glue::glue("cog_departement_{format(Sys.time(), '%Y')}"), "n/a", "DEPARTEMENT", "22"),
    COMMUNE = c("Code de la commune", glue::glue("cog_commune_{format(Sys.time(), '%Y')}"), "n/a", "COMMUNE", "22"),
    EPCI = c("Code de l'EPCI", glue::glue("cog_epci_{format(Sys.time(), '%Y')}"), "n/a", "EPCI", "22"),
    IRIS = c("Code de l'iris", glue::glue("cog_iris_{format(Sys.time(), '%Y')}"), "n/a", "IRIS", "22"),
    ANNEE = c("Millésime des données", "annee", "n/a", "ANNEE", "2022"),
    MOIS = c("Mois des données", "mois", "n/a", "MOIS", "2022-01"),
  )

  result <- dido_csv(data)

  expect_equal(result, expected)
})

test_that("dido_csv works for default types", {
  data <- tibble::tibble(
    CHAR = c("a"),
    INTEGER = c(1),
    NUMBER = c(1.2)
  )
  expected <- tibble::tibble(
    CHAR = c("CHAR", "texte", "n/a", "CHAR", "a"),
    INTEGER = c("INTEGER", "entier", "s/u", "INTEGER", "1"),
    NUMBER = c("NUMBER", "nombre", "s/u", "NUMBER", "1.2"),
  )

  result <- dido_csv(data)

  expect_equal(result, expected)
})

test_that("dido_csv works with params", {
  params <- list(
    COL = list(name = "COLUMN", unit = "unit", description = "description", type = "nombre")
  )
  data <- tibble::tibble(
    COL = c("secret")
  )
  expected <- tibble::tibble(
    COL = c("description", "nombre", "unit", "COLUMN", "secret")
  )

  result <- dido_csv(data, params)

  expect_equal(result, expected)
})

test_that("dido_csv works with column name with space", {
  params <- list(
    COL = list(name = "COLUMN WITH SPACE IN NAME", unit = "unit", description = "description", type = "nombre")
  )
  data <- tibble::tibble(
    COL = c("secret")
  )
  expected <- tibble::tibble(
    COL = c("description", "nombre", "unit", "COLUMN_WITH_SPACE_IN_NAME", "secret")
  )

  result <- dido_csv(data, params)

  expect_equal(result, expected)
})


test_that("dido_csv works with regexp params", {
  params <- list(
    "COL" = list(unit = "test", description = "col", type = "unit"),
    `COL.*` = list(unit = "unit", description = "description", type = "nombre")
  )
  data <- tibble::tibble(
    COL_2021 = c("secret"),
    COL_2022 = c("secret"),
    COL = c("secret")
  )
  expected <- tibble::tibble(
    COL_2021 = c("description", "nombre", "unit", "COL_2021", "secret"),
    COL_2022 = c("description", "nombre", "unit", "COL_2022", "secret"),
    COL = c("col", "unit", "test", "COL", "secret")
  )

  result <- dido_csv(data, params)

  expect_equal(result, expected)
})

test_that("dido_csv works with glue evaluation", {
  params <- list(
    `COL.*` = list(unit = "unit", description = "{tolower(name)}", type = "nombre")
  )
  data <- tibble::tibble(
    COL_2021 = c("secret"),
    COL_2022 = c("secret"),
  )
  expected <- tibble::tibble(
    COL_2021 = c("col_2021", "nombre", "unit", "COL_2021", "secret"),
    COL_2022 = c("col_2022", "nombre", "unit", "COL_2022", "secret"),
  )

  result <- dido_csv(data, params)

  expect_equal(result, expected)
})

test_that("dido_csv warns on type 'nombre'", {
  data <- tibble::tibble(
    CHAR = c("a"),
    INTEGER = c(1),
    NUMBER = c(1.2)
  )
  expected <- tibble::tibble(
    CHAR = c("CHAR", "texte", "n/a", "CHAR", "a"),
    INTEGER = c("INTEGER", "entier", "s/u", "INTEGER", "1"),
    NUMBER = c("NUMBER", "nombre", "s/u", "NUMBER", "1.2"),
  )

  expect_warning(dido_csv(data), regexp = "Vous utilisez un type `nombre` dans vos entêtes")

  params = list(
    NUMBER = list(type = "nombre(1)")
  )
  expect_warning(dido_csv(data, params = params), regexp = NA)
})

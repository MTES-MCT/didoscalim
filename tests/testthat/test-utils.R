test_that("check_argument is ok", {
  fn <- function(arg) {
    check_mandatory_arguments(c("arg"))
  }
  expect_error(fn(), "obligatoire")
  expect_error(fn(NULL), "obligatoire")
  expect_equal(fn("string"), TRUE)

  fn <- function(arg1, arg2) {
    check_mandatory_arguments("arg1", "arg2")
  }
  expect_error(fn(), "obligatoire")
  expect_equal(fn("arg1", "arg2"), TRUE)
})

test_that("find_by_column fails if data empty", {
  expect_error(find_by_column(tibble(), "string", "col", return = c("id")), "Impossible de trouver")
})

test_that("find_by_column fails if more than one row is returned", {
  data <- tibble::tibble(col = c("a","a"))
  expect_error(find_by_column(data, "a", "col", return = c("id")), "trop ou pas assez")
})

test_that("find_by_column fails if no row is returned", {
  data <- tibble::tibble(col = c("a","a"))
  expect_error(find_by_column(data, "b", "col", return = c("id")), "retourne")
})

test_that("find_by_columns return default column", {
  data <- tibble::tibble(col1 = c("a","b"), id = c("d","e"))
  result <- tibble::tibble(id = c("d"))

  expect_equal(find_by_column(data, "a", "col1"), result)
})


test_that("find_by_columns return requested columns", {
  data <- tibble::tibble(col1 = c("a","b"), col2 = c("d","e"))
  result <- tibble::tibble(col1 = c("a"), col2 = c("d"))

  expect_equal(find_by_column(data, "a", "col1", return = c("col1", "col2")), result)
})

test_that("add_columns_if_empty", {
  result <- add_columns_if_empty(
    tibble(a = character()),
    c("id", "title")
  )
  expect_equal(names(result), c("a", "id", "title"))

  result <- add_columns_if_empty(
    tibble(),
    c("col1", "col2")
  )
  expect_equal(names(result), c("col1", "col2"))
})

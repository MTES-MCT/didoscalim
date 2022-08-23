test_that("abort_bad_argument works", {
  expect_error(abort_bad_argument("arg"),
    "`arg` est obligatoire et ne peut être null",
    class = "error_bad_argument"
  )
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

test_that("compare_title works", {
  column <- tibble(title = c("a string with   éà", "a STRING with éà", "another string"))

  expect_equal(compare_title(column$title, "a string WITH Éà."), c(TRUE, TRUE, FALSE))
})

test_that("compare_title fails on bad option", {
  withr::local_options(list(didoscalim_title_comparison = "aaa"))

  expect_error(compare_title("", ""), class = "bad_argument")
})

test_that("compare_title fails on bad option", {
  column <- tibble(title = c("a", "a", "aa"))

  withr::local_options(list(didoscalim_title_comparison = function(column, string) {
      nchar(column) == nchar(string)
    })
  )

  expect_equal(compare_title(column$title, "b"), c(TRUE, TRUE, FALSE))
})

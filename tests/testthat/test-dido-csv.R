test_that("dido_csv works", {
  data <- tribble(
    ~name1, ~name2,
    "a",   1
  )
  expected <- tribble(
    ~name1, ~name2,
    "name1", "name2",
    "texte", "entier",
    "n/a", "s/u",
    "NAME1", "NAME2",
    "a",   "1"
  )

  result <- dido_csv(data)

  expect_equal(result, expected)
})

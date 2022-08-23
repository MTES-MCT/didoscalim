test_that("dido_write_csv works", {
  tmp <- tempfile()
  on.exit(unlink(tmp))

  data <- tibble(a = c("b", 1), c = c("d", 2))
  dido_write_csv(data, tmp)

  expect_equal(readr::read_file(tmp), '\"b\";\"d\"\n\"1\";\"2\"\n')
})

test_that("dido_write_csv fails on missing params", {
  expect_error(dido_write_csv(), "obligatoire")
  expect_error(dido_write_csv(tibble()), "obligatoire")
})


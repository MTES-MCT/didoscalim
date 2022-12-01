test_that("expect_datetime works", {
  expect_datetime("2022-01-01T00:00:00", "2022-01-01 00:00:00")

  expect_datetime("2022-01-01T02:00:00Z", "2022-01-01T03:00:00+01:00")

  expect_datetime("2022-01-01T02:00:00+00:00", "2022-01-01T02:00:10Z", delta = 10)

  expect_error(expect_datetime("2022-01-01T00:00:00", "2022-01-01T01:00:00"),
               "(delta 3600s greater than 0s)")
})


test_that("check list_datasets errors on missing param", {
  expect_error(get_dataset(), class = "error_bad_argument")
})

test_that("check list_datasets works", {
  skip_unless_dev_env()

  dataset <- add_or_update_dataset(
    title = "dataset title",
    description = "test",
    topic = "Transports",
    frequency = "unknown",
  )

  ds <- expect_s3_class(list_datasets(), "tbl")
  expect_true("id" %in% names(ds))
})

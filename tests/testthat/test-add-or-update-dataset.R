test_that("check add_or_update_dataset works", {
  skip_unless_dev_env()

  dataset_title <- "didoscalim ds check add_or_update_dataset works"

  datasets <- list_datasets()
  if (nrow(datasets) > 0) {
    ds <- datasets %>% filter(title == dataset_title)
    for (i in ds$id) delete_dataset(i)
  }

  dataset <- add_or_update_dataset(
    title = dataset_title,
    description = "test",
    topic = "Transports",
    frequency = "unknown",
    temporal_coverage_start = "2020-01-01",
    temporal_coverage_end = "2020-12-31"
  )

  expect_s3_class(dataset, "dido_dataset")
  expect_equal(dataset$temporal_coverage$start, "2020-01-01")
  expect_equal(dataset$temporal_coverage$end, "2020-12-31")

  dataset <- add_or_update_dataset(
    title = dataset_title,
    description = "test",
    topic = "Transports",
    frequency = "unknown",
    temporal_coverage_start = "2021-01-01",
    temporal_coverage_end = "2021-12-31"
  )

  expect_s3_class(dataset, "dido_dataset")
  expect_equal(dataset$temporal_coverage$start, "2021-01-01")
  expect_equal(dataset$temporal_coverage$end, "2021-12-31")
})

test_that("check add_or_update_dataset fails when two many datasets", {
  skip_unless_dev_env()

  dataset <- add_dataset(
    title = "didoscalim ds check add_or_update_dataset failed",
    description = "test",
    topic = "Transports",
    frequency = "unknown",
  )

  dataset <- add_dataset(
    title = "didoscalim ds check add_or_update_dataset failed",
    description = "test",
    topic = "Transports",
    frequency = "unknown",
  )

  expect_error(add_or_update_dataset(
    title = "didoscalim ds check add_or_update_dataset failed",
    description = "test",
    topic = "Transports",
    frequency = "unknown",
  ), "doit contenir une ligne.")

})

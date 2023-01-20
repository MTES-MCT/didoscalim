dataset_title <- "didoscalim ds check add_or_update_dataset"

list_datasets() %>%
  filter(grepl(dataset_title, title)) %>%
  purrr::pwalk(~ delete_dataset(.))

test_that("check add_or_update_dataset works for creation", {
  skip_unless_dev_env()

  dataset <- add_or_update_dataset(
    title = glue::glue("{ dataset_title } works for creation"),
    description = "test",
    topic = "Transports",
    frequency = "unknown",
    temporal_coverage_start = "2020-01-01",
    temporal_coverage_end = "2020-12-31"
  )

  expect_s3_class(dataset, "dido_dataset")
  expect_equal(dataset$temporal_coverage$start, "2020-01-01")
  expect_equal(dataset$temporal_coverage$end, "2020-12-31")
})

test_that("check add_or_update_dataset works for update", {
  skip_unless_dev_env()

  dataset <- add_or_update_dataset(
    title = glue::glue("{ dataset_title } works for update"),
    description = "test",
    topic = "Transports",
    frequency = "unknown",
    temporal_coverage_start = "2021-01-01",
    temporal_coverage_end = "2021-12-31"
  )

  expect_s3_class(dataset, "dido_dataset")
  expect_equal(dataset$temporal_coverage$start, "2021-01-01")
  expect_equal(dataset$temporal_coverage$end, "2021-12-31")

  dataset <- add_or_update_dataset(
    title = glue::glue("{ dataset_title } works for update ."),
    description = "test",
    topic = "Transports",
    frequency = "unknown",
  )

  expect_s3_class(dataset, "dido_dataset")
  expect_equal(dataset$temporal_coverage$start, "2021-01-01")
  expect_equal(dataset$temporal_coverage$end, "2021-12-31")
  expect_equal(dataset$title, glue::glue("{ dataset_title } works for update ."))

  with_options(
    didoscalim_update_only = TRUE,
    {
      expect_error(
        dataset <- add_or_update_dataset(
          title = "just a bad title",
          description = "test",
          topic = "Transports",
          frequency = "unknown",
        ),
        class = "error_update_only"
      )
    })


})

test_that("check add_or_update_dataset does nothing if no change", {
  skip_unless_dev_env()

  origin <- add_or_update_dataset(
    title = glue::glue("{dataset_title} with no change"),
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  dataset <- add_or_update_dataset(
    title = glue::glue("{dataset_title} with no change"),
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  expect_equal(origin, dataset)
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
  ), "plusieurs datasets avec le titre")
})

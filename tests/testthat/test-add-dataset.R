test_that("add_dataset works", {
  skip_unless_dev_env()

  dataset <- add_dataset(
    title = "didoscalim check add_dataset works",
    description = "test",
    topic = "Transports",
    frequency = "unknown",
    tags = list("agenda-21", "agriculture")
  )

  expect_s3_class(dataset, "dido_dataset")
  expect_equal(dataset$title, "didoscalim check add_dataset works")
  expect_equal(dataset$description, "test")
  expect_equal(dataset$topic, "Transports")
  expect_equal(dataset$frequency, "unknown")
  expect_equal(dataset$license, "fr-lo")
  expect_equal(dataset$tags, list("agenda-21", "agriculture"))
})

test_that("add_dataset complete works", {
  skip_unless_dev_env()

  dataset <- add_dataset(
    title = "didoscalim check add_dataset complete works",
    description = "test",
    topic = "Transports",
    tags = list("agenda-21", "agriculture"),
    frequency = "annual",
    frequency_date = "2022-12-31",
    temporal_coverage_start = "2021-01-01",
    temporal_coverage_end = "2021-12-31",
    granularity = "fr:region",
    zones = "country:fr",
    caution = "caution",
    license = "ODbL-1.0"
  )

  expect_s3_class(dataset, "dido_dataset")
  expect_equal(dataset$title, "didoscalim check add_dataset complete works")
  expect_equal(dataset$description, "test")
  expect_equal(dataset$topic, "Transports")
  expect_equal(dataset$tags, list("agenda-21", "agriculture"))
  expect_equal(dataset$frequency, "annual")
  expect_equal(dataset$frequency_date, "2022-12-31T00:00:00+00:00")
  expect_equal(dataset$temporal_coverage$start, "2021-01-01")
  expect_equal(dataset$temporal_coverage$end, "2021-12-31")
  expect_equal(dataset$spatial$granularity, "fr:region")
  expect_equal(dataset$spatial$zones, list("country:fr"))
  expect_equal(dataset$caution, "caution")
  expect_equal(dataset$license, "ODbL-1.0")
})


test_that("add_dataset fails correctly", {
  skip_unless_dev_env()

  err <- expect_error(
    add_dataset(
      title = "didoscalim ds add_dataset fails correctly",
      description = "test",
      topic = "Transports",
      frequency = "annual"
    ),
    class = "api_error"
  )
  expect_match(err$message, "Erreur de validation")
})

test_that("add_dataset errors on missing param", {
  expect_error(add_dataset(description = "test", topic = "Transports", frequency = "unknown"), "is missing")
  expect_error(add_dataset(title = "test", topic = "Transports", frequency = "unknown"), "is missing")
  expect_error(add_dataset(title = "test", description = "test", frequency = "unknown"), "is missing")
  expect_error(add_dataset(title = "test", description = "test", topic = "Transports"), "is missing")
})

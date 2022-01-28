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
  expect_equal(dataset$license, "fr-lo")
})

test_that("add_dataset complete works", {
  skip_unless_dev_env()

  dataset <- add_dataset(
    title = "didoscalim check add_dataset complete works",
    description = "test",
    topic = "Transports",
    tags = list("agenda-21", "agriculture"),
    frequency = "annual",
    frequency_date =  "2022-12-31",
    temporal_coverage_start = "2021-01-01",
    temporal_coverage_end = "2021-12-31",
    granularity = "fr:region",
    zones = "country:fr",
    caution = "caution",
    license = "ODbL-1.0"
  )

  expect_s3_class(dataset, "dido_dataset")
  expect_equal(dataset$title, "didoscalim check add_dataset complete works")
  expect_equal(dataset$license, "ODbL-1.0")
  expect_equal(dataset$spatial, list(granularity = "fr:region", zones = list("country:fr")))
  expect_equal(dataset$temporal_coverage, list(end = "2021-12-31", start = "2021-01-01"))
})


test_that("add_dataset fails correctly", {
  skip_unless_dev_env()

  err <- rlang::catch_cnd(
    add_dataset(
      title = "didoscalim ds add_dataset fails correctly",
      description = "test",
      topic = "Transports",
      frequency = "annual"
    )
  )

  expect_s3_class(err, "api_error")
  expect_match(err$message, "Erreur de validation")
})

test_that("add_dataset errors on missing param", {
  expect_error(add_dataset(description = "test", topic = "Transports", frequency = "unknown"), "is missing")
  expect_error(add_dataset(title = "test", topic = "Transports", frequency = "unknown"), "is missing")
  expect_error(add_dataset(title = "test", description = "test", frequency = "unknown"), "is missing")
  expect_error(add_dataset(title = "test", description = "test", topic = "Transports"), "is missing")
})

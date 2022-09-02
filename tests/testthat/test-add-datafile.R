dataset <- add_or_update_dataset(
  title = "didoscalim ds create datafiles works",
  description = "test",
  topic = "Transports",
  frequency = "unknown"
)

test_that("create datafiles works", {
  skip_unless_dev_env()

  date_published <- format(Sys.time(), "%Y-%m-%dT00:00:00+00:00")

  created_df <- add_datafile(
    dataset = dataset,
    title = "didoscalim df create datafiles work",
    description = "description",
    file_name = "dido-csv-simple.csv"
  )

  datafile <- get_datafile(created_df$result$rid)

  expect_equal(datafile$title, "didoscalim df create datafiles work")
  expect_equal(datafile$description, "description")
  expect_equal(datafile$published, date_published)

  millesime <- datafile$millesimes_info[[1]]
  expect_equal(millesime$millesime, format(Sys.time(), "%Y-%m"))
  # publication date/time is correct
  expect_true((ymd_hms(list_millesimes(datafile)[["date_diffusion"]]) - lubridate::now()) < 10)



  date_diffusion <- "2020-01-01 00:00:00"
  created_df2 <- add_datafile(
    dataset = dataset,
    title = "didoscalim df create datafiles work",
    description = "description",
    file_name = "dido-csv-simple.csv",
    temporal_coverage_start = "2021-01-01",
    temporal_coverage_end = "2021-12-31",
    legal_notice = "something",
    millesime = "2020-10",
    date_diffusion = date_diffusion,
  )

  datafile <- get_datafile(created_df2$result$rid)

  expect_equal(datafile$title, "didoscalim df create datafiles work")
  expect_equal(datafile$description, "description")
  expect_equal(datafile$published, date_published)
  expect_equal(datafile$legal_notice, "something")
  expect_equal(datafile$temporal_coverage$start, "2021-01-01")
  expect_equal(datafile$temporal_coverage$end, "2021-12-31")

  millesime <- datafile$millesimes_info[[1]]
  expect_equal(millesime$millesime, "2020-10")

  # publication date/time is correct
  expect_true((ymd_hms(list_millesimes(datafile)[["date_diffusion"]]) - ymd_hms(date_diffusion, tz = Sys.timezone())) < 10)
})

test_that("create datafiles warns when missing param", {
  skip_unless_dev_env()

  dataset <- add_dataset(
    title = "didoscalim ds create datafiles warns when missing param",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  expect_error(add_datafile(title = "test", description = "test", file_name = "test.csv"), class = "error_bad_argument")
  expect_error(add_datafile(dataset = "61f8033c404a870027fb3af4", description = "test", file_name = "test.csv"), class = "error_bad_argument")
  expect_error(add_datafile(dataset = dataset, description = "test", file_name = "test.csv"), class = "error_bad_argument")
  expect_error(add_datafile(dataset = dataset, title = "test", file_name = "test.csv"), class = "error_bad_argument")
  expect_error(add_datafile(dataset = dataset, title = "test", description = "test"), class = "error_bad_argument")
})

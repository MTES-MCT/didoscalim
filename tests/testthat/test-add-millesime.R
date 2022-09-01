test_that("add millesime works", {
  skip_unless_dev_env()

  dataset <- add_dataset(
    title = "didoscalim ds add millesime works",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  job_datafile <- add_datafile(
    dataset = dataset,
    title = "didoscalim df add millesime works",
    description = "description",
    file_name = "dido-csv-simple.csv"
  )

  date_diffusion <- "2020-01-01 00:00:00"
  job_millesime <- add_millesime(
    datafile = get_datafile(get_datafile_rid(job_datafile)),
    millesime = "2012-10",
    file_name = "dido-csv-simple.csv",
    date_diffusion = date_diffusion
  )

  expect_s3_class(job_millesime, "dido_job")
  expect_equal(job_millesime$data$datafile_millesime, "2012-10")

  # date diffusion
  date_diffusion_millesime <- (list_millesimes(job_millesime) %>% filter(millesime == "2012-10"))[["date_diffusion"]]
  expect_true((ymd_hms(date_diffusion_millesime) - ymd_hms(date_diffusion, tz = Sys.timezone())) < 10)

  list_millesimes(job_millesime)

  df <- get_datafile(get_datafile_rid(job_datafile))
  expect_equal(df$millesimes, 2)
})

test_that("add_millesime with errors", {
  skip_unless_dev_env()

  millesime <- "2000-01"

  list_datasets() %>%
    filter(grepl("didoscalim ds add millesime works", title)) %>%
    purrr::pwalk(~ delete_dataset(.))

  dataset <- add_dataset(
    title = "didoscalim ds add millesime exists",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  datafile <- add_datafile(
    dataset = dataset,
    title = "didoscalim df add millesime exists",
    description = "description",
    file_name = "dido-csv-simple.csv",
    millesime = millesime,
  )

  expect_error(
    add_millesime(
      datafile = datafile,
      millesime = millesime,
      file_name = "dido-csv-simple.csv"
    ),
    class = "millesime_exists"
  )

  expect_error(
    add_millesime(
      datafile = datafile,
    ),
    class = "error_bad_argument"
  )
})

test_that("add_millesime errors on missing params", {
  expect_error(add_millesime(datafile = new_dido_datafile(list())), "obligatoire")
  expect_error(add_millesime(file_name = "dido-csv-simple.csv"), "obligatoire")
})

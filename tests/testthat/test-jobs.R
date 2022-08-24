dataset <- add_or_update_dataset(
  title = "didoscalim ds wait_for_jobs works",
  description = "test",
  topic = "Transports",
  frequency = "unknown"
)

test_that("wait_for_jobs works", {
  skip_unless_dev_env()

  datafile_job <- add_datafile(
    dataset = dataset,
    title = "didoscalim df wait_for_jobs works",
    description = "description",
    file = "dido-csv-simple.csv"
  )

  result <- wait_for_job(datafile_job$id)

  expect_s3_class(result, "dido_job")
  expect_true("result" %in% names(result))
  expect_true("data" %in% names(result))
})

test_that("list_jobs works", {
  skip_unless_dev_env()

  jobs <- list_jobs()

  expect_s3_class(jobs, "tbl")
})

test_that("wait_for_jobs works", {
  skip_unless_dev_env()

  datafile <- dido_datafile(
    dataset = dataset,
    title = "un titre bidon",
    description = "une description"
  )
  datafile$tokenFile <- dido_upload_file("dido-csv-with-invalid-data.csv")

  id <- get_dataset_id(dataset)
  job <- dido_job(
    dido_api(
      method = "POST",
      path = glue::glue("/datasets/{id}/datafiles"),
      body = body <- jsonlite::toJSON(datafile, pretty = TRUE, auto_unbox = TRUE, na = "null")
    )
  )

  expect_error(wait_for_job(job), "Il y a des erreurs lors du traitement")
})

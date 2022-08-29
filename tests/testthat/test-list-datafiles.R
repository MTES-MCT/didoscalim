dataset_title <- "didoscalim ds check list_datafiles works"
datafile_title <- "didoscalim df check list_datafiles works"

list_datasets() %>%
  filter(title == dataset_title) %>%
  purrr::pmap(~delete_dataset(.))

test_that("list_datafiles works when no datasets", {
  skip_unless_dev_env()

  df <- list_datafiles()
  expect_s3_class(df, "tbl")
})

test_that("list_datafiles works", {
  skip_unless_dev_env()

  dataset <- add_or_update_dataset(
    title = dataset_title,
    description = "Description des données statistiques",
    topic = "Transports",
    frequency = "unknown"
  )

  result <- add_or_update_datafile(
    dataset,
    title = datafile_title,
    description = "Un fichier de données de test",
    file_name = dido_example("augmente.csv"),
    millesime = "2001-01"
  )

  df <- list_datafiles()
  expect_s3_class(df, "tbl")
})

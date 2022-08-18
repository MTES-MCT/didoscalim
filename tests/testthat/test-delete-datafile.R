test_that("delete millesime works", {
  skip_unless_dev_env()

  dataset <- add_dataset(
    title = "didoscalim ds delete millesime works",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  datafile <- add_datafile(
    dataset = dataset,
    title = "didoscalim df delete millesime works",
    description = "description",
    file_name = "dido-csv-simple.csv"
  )

  result <- delete_datafile(datafile)

  expect_equal(result, TRUE)
  expect_error(get_datafile(datafile), "Impossible de trouver un fichier")
})

test_that("delete_millesime errors on missing params", {
  expect_error(delete_datafile(), "obligatoire")
})

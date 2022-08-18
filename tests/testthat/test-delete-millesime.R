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

  add_millesime(
    datafile = datafile,
    file_name = "dido-csv-simple.csv",
    millesime = "2000-10"
  )

  df <- get_datafile(datafile$result$rid)

  nb_of_millesimes <- df$millesimes
  millesime_name <- df$millesimes_info[[1]]$millesime

  millesime <- delete_millesime(
    datafile = df,
    millesime = millesime_name
  )

  df <- get_datafile(datafile$result$rid)
  expect_equal(df$millesimes, 1)
})

test_that("delete_millesime errors on missing params", {
  expect_error(delete_millesime(millesime = "2020-10"), "obligatoire")
  expect_error(delete_millesime(datafile = "123e4567-e89b-12d3-a456-426614174000"), "obligatoire")
})

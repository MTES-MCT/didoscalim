test_that("replace millesime works", {
  skip_unless_dev_env()

  dataset <- add_dataset(
    title = "didoscalim ds replace millesime works",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  datafile <- add_datafile(
    dataset = dataset,
    title = "didoscalim df replace millesime works",
    description = "description",
    file_name = "dido-csv-simple.csv"
  )

  df <- get_datafile(datafile$result$rid)

  nb_of_millesimes <- df$millesimes
  millesime_name <- df$millesimes_info[[1]]$millesime

  millesime <- replace_millesime(
    datafile = df,
    file_name = "dido-csv-simple.csv",
    millesime = millesime_name
  )

  df <- get_datafile(datafile$result$rid)
  expect_equal(millesime$data$datafile_millesime, millesime_name)
  expect_equal(df$millesimes, nb_of_millesimes)
})

test_that("replace_millesime errors on missing params", {
  expect_error(replace_millesime(datafile = "123e4567-e89b-12d3-a456-426614174000", millesime = "2020-10"), "obligatoire")
  expect_error(replace_millesime(millesime = "2020-10", file_name = "dido-csv-simple.csv"), "obligatoire")
  expect_error(replace_millesime(
    datafile = new_dido_datafile(list(rid = "123e4567-e89b-12d3-a456-426614174000")),
    file_name = "dido-csv-simple.csv"
  ), "obligatoire")
  expect_error(replace_millesime(
    datafile = new_dido_datafile(list(rid = "123e4567-e89b-12d3-a456-426614174000")),
    millesime = "2021-10"
  ), "obligatoire")
})

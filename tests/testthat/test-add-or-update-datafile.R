test_that("find_millesimes_to_delete works", {
  millesimes <- tibble(millesime = c("2020-01", "2020-02"))
  result <- find_millesimes_to_delete(millesimes, keep_last_n = 0)
  expect_equal(nrow(result), 2, info = "return the two lines")

  millesimes <- tibble(millesime = c("2020-01", "2020-02"))
  result <- find_millesimes_to_delete(millesimes, keep_last_n = 1)
  expect_equal(nrow(result), 1, info = "return one line")
  expect_equal(result$millesime, "2020-01", info = "return the older millesime")

  millesimes <- tibble(millesime = c("2020-01", "2020-01"))
  result <- find_millesimes_to_delete(millesimes, keep_last_n = 1)
  expect_equal(nrow(result), 1, info = "return one line")
  expect_equal(result$millesime, "2020-01", info = "return the older millesime")

  millesimes <- tibble(millesime = c("2020-01", "2020-02"))
  result <- find_millesimes_to_delete(millesimes, keep_last_n = 2)
  expect_equal(nrow(result), 0, info = "return an empty tibble")

  millesimes <- tibble(millesime = c("2020-01", "2020-02"))
  result <- find_millesimes_to_delete(millesimes, keep_last_n = Inf)
  expect_equal(nrow(result), 0, info = "return an empty tibble")
})

test_that("check add_or_update_datafile works", {
  skip_unless_dev_env()

  dataset_title <- "didoscalim ds check add_or_update_datafile works"
  datafile_title <- "didoscalim df check add_or_update_datafile works"

  datasets <- list_datasets()
  if (nrow(datasets) > 0) {
    ds <- datasets %>% filter(title == dataset_title)
    for (i in ds$id) delete_dataset(i)
  }

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

  result <- add_or_update_datafile(
    dataset,
    title = datafile_title,
    description = "Un fichier de données de test",
    file_name = dido_example("augmente.csv"),
    temporal_coverage_start = "2000-01-01",
    temporal_coverage_end = "2001-12-31",
    millesime = "2002-01"
  )
  datafile <- get_datafile(result)
  expect_equal(datafile$millesimes, 2, info = "datafile should have 2 millesimes")
  expect_equal(datafile$temporal_coverage$start, "2000-01-01")
  expect_equal(datafile$temporal_coverage$end, "2001-12-31")

  result <- add_or_update_datafile(
    dataset,
    title = datafile_title,
    description = "Un fichier de données de test",
    file_name = dido_example("augmente.csv"),
    millesime = "2003-01",
    temporal_coverage_end = "2002-12-31",
    legal_notice = "something",
    keep_old_millesimes = 0,
  )
  datafile <- get_datafile(result)
  expect_equal(datafile$millesimes, 1, info = "datafile should have 1 millesimes")
  expect_equal(datafile$legal_notice, "something")
  expect_equal(datafile$temporal_coverage$start, "2000-01-01")
  expect_equal(datafile$temporal_coverage$end, "2002-12-31")
})


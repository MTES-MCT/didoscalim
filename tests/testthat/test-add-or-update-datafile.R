dataset_title <- "didoscalim ds check add_or_update_datafile"
datafile_title <- "didoscalim df check add_or_update_datafile"

test_that("find_millesimes_to_delete works", {
  millesimes <- tibble(millesime = c("2020-01", "2020-02", "2020-03"))
  result <- find_millesimes_to_delete(millesimes, keep_old_millesimes = Inf, "2020-03")
  expect_equal(nrow(result), 0, info = "we want to keep all millesimes : should return an empty tibble")

  millesimes <- tibble(millesime = c("2020-01", "2020-02", "2020-03"))
  result <- find_millesimes_to_delete(millesimes, keep_old_millesimes = 1, "2020-02")
  expect_equal(nrow(result), 0, info = "we update the middle millesime and keep 1 : should return an empty tibble")

  millesimes <- tibble(millesime = c("2020-03", "2020-02", "2020-01"))
  result <- find_millesimes_to_delete(millesimes, keep_old_millesimes = 0, "2020-02")
  expect_equal(nrow(result), 1, info = "we update the intermediate millesime and keep 0 old : should return a tibble with the older millesime")
  expect_equal(result$millesime, "2020-01", info = "return the older millesime")
})

test_that("check add_or_update_datafile works", {
  skip_unless_dev_env()

  list_datasets() %>%
    filter(title == dataset_title) %>%
    purrr::pwalk(~ delete_dataset(.))

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

test_that("check add_or_update_datafile when millesime exists", {
  skip_unless_dev_env()

  list_datasets() %>%
    filter(title == dataset_title) %>%
    purrr::pwalk(~ delete_dataset(.))

  dataset <- add_or_update_dataset(
    title = dataset_title,
    description = "Description des données statistiques",
    topic = "Transports",
    frequency = "unknown"
  )

  # check creation works
  add_or_update_datafile(
    dataset,
    title = datafile_title,
    description = "Un fichier de données de test",
    file_name = dido_example("augmente.csv"),
    millesime = "2001-01",
  ) %>%
    expect_s3_class("dido_job")

  # check error if millesime exists
  expect_error(
    add_or_update_datafile(
      dataset,
      title = datafile_title,
      description = "Un fichier de données de test",
      file_name = dido_example("augmente.csv"),
      millesime = "2001-01",
      on_existing_millesime = "fail"
    ),
    class = "millesime_exists"
  )

  # check skip option works
  add_or_update_datafile(
    dataset,
    title = datafile_title,
    description = "Un fichier de données de test",
    file_name = dido_example("augmente.csv"),
    millesime = "2001-01",
    on_existing_millesime = "skip"
  ) %>%
    expect_equal(NULL, info = "should return NULL") %>%
    expect_message("existe")

  # check replace option works
  add_or_update_datafile(
    dataset,
    title = datafile_title,
    description = "Un fichier de données de test",
    file_name = dido_example("augmente.csv"),
    millesime = "2001-01",
    on_existing_millesime = "replace"
  ) %>%
    expect_s3_class("dido_job")

  # check replace option works and we can change title
  new_datafile_title <- paste0(datafile_title, " .")
  result <- add_or_update_datafile(
    dataset,
    title = new_datafile_title,
    description = "Un fichier de données de test",
    file_name = dido_example("augmente.csv"),
    millesime = "2001-01",
    on_existing_millesime = "replace"
  )
  expect_s3_class(result, "dido_job")
  datafile <- get_datafile(result)
  expect_equal(datafile$title, new_datafile_title)


  # check we fail for new datafile when option didoscalim_update_only is TRUE
  with_options(
    didoscalim_update_only = TRUE,
    {
      expect_error(
        add_or_update_datafile(
          dataset,
          title = "another title",
          description = "Un fichier de données de test",
          file_name = dido_example("augmente.csv"),
        ),
        class = "error_update_only"
      )
    })
})


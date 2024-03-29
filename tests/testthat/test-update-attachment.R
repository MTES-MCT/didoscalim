test_that("update_attachment works", {
  skip_unless_dev_env()

  att_title <- glue::glue("didoscalim df update_attachment works {generate_random_string()}")
  dataset <- add_dataset(
    title = "didoscalim ds get_attachment works",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  att <- add_attachment(
    dataset = dataset,
    title = att_title,
    description = "test",
    file_name = "dido-csv-simple.csv"
  )

  att$description <- "une autre description"
  att <- update_attachment(att)
  expect_equal(att$description, "une autre description")

  att$type <- "historical_data"
  att <- update_attachment(att)
  expect_equal(att$type, "historical_data")
})

test_that("update_attachment errors on missing params", {
  expect_error(update_attachment(), "obligatoire")
  expect_error(update_attachment(tibble()), "pas du type attendu")
})

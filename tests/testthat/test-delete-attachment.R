test_that("delete attachment works", {
  skip_unless_dev_env()

  dataset <- add_or_update_dataset(
    title = "didoscalim ds delete millesime works",
    description = "test",
    topic = "Transports",
    frequency = "unknown"
  )

  attachment <- add_or_update_attachment(
    dataset = dataset,
    title = "didoscalim df delete millesime works",
    description = "description",
    file_name = "dido-csv-simple.csv"
  )

  result <- delete_attachment(attachment)

  expect_equal(result, TRUE)
  expect_error(get_attachment(attachment), "Impossible de trouver un fichier")
})

test_that("delete_millesime errors on missing params", {
  expect_error(delete_attachment(), "obligatoire")
})

test_that("delete_millesime errors on missing params", {
  expect_error(delete_attachment("aaaaa"), "pas du type attendu")
})

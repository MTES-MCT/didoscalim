test_that("check add_or_update_attachment works", {
  skip_unless_dev_env()

  dataset_title <- "didoscalim ds check add_or_update_attachment works"
  attachment_title <- "didoscalim att check add_or_update_attachment works"

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

  result <- add_or_update_attachment(
    dataset,
    title = attachment_title,
    description = "Un fichier de données de test",
    file_name = dido_example("augmente.csv")
  )
  expect_s3_class(result, "dido_attachment")
  expect_equal(length(get_dataset(dataset)$attachments), 1)

  result <- add_or_update_attachment(
    dataset,
    title = attachment_title,
    description = "Un fichier de données de test",
    file_name = dido_example("augmente.csv"),
    published = "2018-01-01"
  )

  expect_s3_class(result, "dido_attachment")
  expect_equal(length(get_dataset(dataset)$attachments), 1)

  # check we can change title
  new_attachment_title <- paste0(attachment_title, ".")
  result <- add_or_update_attachment(
    dataset,
    title = new_attachment_title,
    description = "UN fichier de données de test.",
    file_name = dido_example("augmente.csv"),
    published = "2018-01-01"
  )

  expect_s3_class(result, "dido_attachment")
  expect_equal(length(get_dataset(dataset)$attachments), 1)
  expect_equal(result$title, new_attachment_title)

  # check we can change type
  result <- add_or_update_attachment(
    dataset,
    title = new_attachment_title,
    type = "historical_data",
    description = "UN fichier de données de test.",
    file_name = dido_example("augmente.csv"),
    published = "2018-01-01"
  )

  expect_s3_class(result, "dido_attachment")
  expect_equal(length(get_dataset(dataset)$attachments), 1)
  expect_equal(result$type, "historical_data")

  # check we can change type
  result <- add_or_update_attachment(
    dataset,
    title = new_attachment_title,
    description = "UN fichier de données de test.",
    remote_url = "http://un.serveur/",
  )

  expect_s3_class(result, "dido_attachment")
  expect_equal(result$remote, TRUE)


  # check we fail for new attachment when option didoscalim_update_only is TRUE
  with_options(
    didoscalim_update_only = TRUE,
    {
      expect_error(
        add_or_update_attachment(
          dataset,
          title = "another title",
          description = "UN fichier de données de test.",
          file_name = dido_example("augmente.csv"),
          published = "2018-01-01"
        ),
        class = "error_update_only"
      )
    })

})

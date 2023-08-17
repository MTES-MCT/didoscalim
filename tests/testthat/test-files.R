test_that("upload files works", {
  skip_unless_dev_env()

  id <- dido_upload_file(test_path("file-upload.txt"))

  expect_true(nchar(id) > 0)
})

test_that("check_csv return error on txt", {
  skip_unless_dev_env()

  id <- dido_upload_file(test_path("file-upload.txt"))

  expect_error(check_csv(id), class = "invalid_file")
})

test_that("check_csv works with valid file", {
  skip_unless_dev_env()

  id <- dido_upload_file(test_path("dido-csv-valid.csv"))

  expect_true(check_csv(id))
})

test_that("check_csv fails on errors", {
  skip_unless_dev_env()

  id <- dido_upload_file(test_path("dido-csv-with-error.csv"))

  expect_error(check_csv(id), class = "invalid_file")
})

test_that("check_csv works on warnings", {
  skip_unless_dev_env()
  withr::local_options(list(didoscalim_verbosity = "info"))

  id <- dido_upload_file(test_path("dido-csv-with-warning.csv"))

  expect_message(check_csv(id), "Le fichier est valide mais il y a des alertes")
})

test_that("dido_upload_file fails on missing param", {
  err <- expect_error(dido_upload_file(), class = "error_bad_argument")
  expect_match(err$message, "`file_name` est obligatoire et ne peut être null")
})

test_that("dido_upload_file fails on missing file", {
  expect_error(dido_upload_file("no_such_file.csv"), class = "no_such_file")
})

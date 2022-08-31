test_that("check_import_file return TRUE for valid files", {
  # we set a false environment
  withr::local_options(didoscalim_work_env = "OTHER")

  expect_true(check_import_file("dido-csv-valid.csv"), info = "check_import_file returns TRUE")
  expect_equal(get_work_env(), "OTHER", info = "old environment is restored")
})

test_that("check_import_file works return FALSE for invalid files and print message", {
  # we set a false environment
  withr::local_options(didoscalim_work_env = "OTHER")

  expect_message(return <- check_import_file("dido-csv-with-error.csv"), info = "message is ok")
  expect_false(return, FALSE, info = "check_import_file return FALSE on error")
  expect_equal(get_work_env(), "OTHER", info = "old environment is restored")
})

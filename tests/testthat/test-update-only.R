test_that("didoscalim_update_only works", {
  expect_message(didoscalim_update_only(TRUE), "interdite")
  expect_true(getOption("didoscalim_update_only"))
  expect_message(didoscalim_update_only(FALSE), "autorisée")
  expect_false(getOption("didoscalim_update_only"))
})


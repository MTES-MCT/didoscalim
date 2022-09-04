test_that("with_didoscalim_verbosity works", {
  options(didoscalim_verbosity = "info")

  with_didoscalim_verbosity(
    "debug",
    expect_equal(didoscalim_verbosity(), "debug", info = "we should be in debug")
  )
  expect_equal(didoscalim_verbosity(), "info", info = "we should be in info")
})

test_that("with_didoscalim_verbosity works", {
  options(c(didoscalim_verbosity = "info"))

  local_didoscalim_verbosity("debug")
  expect_equal(didoscalim_verbosity(), "debug", info = "we should be in debug")
})


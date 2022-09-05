test_that("my_organization works without params", {
  skip_unless_dev_env()

  org <- my_organization()

  expect_true((nchar(org) > 0))
})

test_that("my_organization works with good params", {
  skip_unless_dev_env()

  org <- my_organization("bsi")

  expect_true((nchar(org) > 0))
})

test_that("my_organization errors if no organization found", {
  skip_unless_dev_env()

  expect_error(my_organization("some random string"), "aucune organisation")
})

test_that("fails on multiple organizations", {
  mockery::stub(my_organization, "me", tibble(organizations = c("org1", "org2")))
  expect_error(my_organization(), "Vous appartenez à plusieurs organisations")

  mockery::stub(my_organization, "me", tibble(organizations = c("org1", "org2")))
  expect_error(my_organization("org"), "La recherche retourne plusieurs organisations")
})

test_that("fails when no organization is return", {
  expect_error(my_organization("some weird org"), "La recherche ne retourne aucune organisation")
})

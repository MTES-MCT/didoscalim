# get_dataset_id tests

id <- "62fced78a1e354516cb26b98"

test_that("get_dataset_id works for string", {
  expect_equal(get_dataset_id(id), id)
})

test_that("get_dataset_id works on good dataset", {
  dataset_with_bad_id <- tibble(id = c(id))
  expect_equal(get_dataset_id(dataset_with_bad_id), id)
})

test_that("get_dataset_id return NULL on bad argument", {
  expect_equal(get_dataset_id(NULL), NULL)
  expect_equal(get_dataset_id("a"), NULL)
})

test_that("get_dataset_id fails on frame without id col", {
  dataset_without_id_col <- tibble(noid = c("truc"))
  expect_error(get_dataset_id(dataset_without_id_col), "doit être soit un id de dataset")
})

test_that("get_dataset_id fails if frame has 2 col", {
  dataset_with_two_lines <- tibble(id = c("truc", "bidule"))
  expect_error(get_dataset_id(dataset_with_two_lines), "contient 2 ligne")
})

test_that("get_dataset_id returns NULL if frame and bad oid", {
  dataset_with_bad_id <- tibble(id = c("truc"))
  expect_equal(get_dataset_id(dataset_with_bad_id), NULL)
})

# get_datafile_rid tests

rid <- "30db7e10-73a8-49fb-b8cf-166a169905f5"

test_that("get_datafile_rid works for string", {
  expect_equal(get_datafile_rid(rid), rid)
})

test_that("get_datafile_rid works on good dataset", {
  dataset_with_bad_id <- tibble(rid = c(rid))
  expect_equal(get_datafile_rid(dataset_with_bad_id), rid)
})

test_that("get_datafile_rid return NULL on bad argument", {
  expect_equal(get_datafile_rid(NULL), NULL)
  expect_equal(get_datafile_rid("a"), NULL)
})

test_that("get_datafile_rid fails on frame without id col", {
  dataset_without_id_col <- tibble(norid = c("truc"))
  expect_error(get_datafile_rid(dataset_without_id_col), "doit être un rid de datafile")
})

test_that("get_datafile_rid fails if frame has 2 col", {
  dataset_with_two_lines <- tibble(rid = c("truc", "bidule"))
  expect_error(get_datafile_rid(dataset_with_two_lines), "contient 2 ligne")
})

test_that("get_datafile_rid returns NULL if frame and bad oid", {
  dataset_with_bad_id <- tibble(rid = c("truc"))
  expect_equal(get_datafile_rid(dataset_with_bad_id), NULL)
})

# get_attachment_rid tests

test_that("get_attachment_rid works for string", {
  expect_equal(get_attachment_rid(rid), rid)
})

test_that("get_attachment_rid works on good dataset", {
  dataset_with_bad_id <- tibble(rid = c(rid))
  expect_equal(get_attachment_rid(dataset_with_bad_id), rid)
})

test_that("get_attachment_rid return NULL on bad argument", {
  expect_equal(get_attachment_rid(NULL), NULL)
  expect_equal(get_attachment_rid("a"), NULL)
})

test_that("get_attachment_rid fails on frame without id col", {
  dataset_without_id_col <- tibble(norid = c("truc"))
  expect_error(get_attachment_rid(dataset_without_id_col), "doit être un rid d'attachment")
})

test_that("get_attachment_rid fails if frame has 2 col", {
  dataset_with_two_lines <- tibble(rid = c("truc", "bidule"))
  expect_error(get_attachment_rid(dataset_with_two_lines), "contient 2 ligne")
})

test_that("get_attachment_rid returns NULL if frame and bad oid", {
  dataset_with_bad_id <- tibble(rid = c("truc"))
  expect_equal(get_attachment_rid(dataset_with_bad_id), NULL)
})

library(testthat)
library(didoscalim)

withr::local_options(list(didoscalim_verbosity = "silent"))
withr::local_options(list(didoscalim_update_only = NULL))
withr::local_options(list(didoscalim_title_comparison = NULL))

test_check("didoscalim")

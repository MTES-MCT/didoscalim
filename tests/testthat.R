library(testthat)
library(didoscalim)

withr::local_options(list(didoscalim_verbosity = "silent"))
test_check("didoscalim")

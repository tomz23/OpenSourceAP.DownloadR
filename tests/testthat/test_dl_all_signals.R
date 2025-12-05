library(testthat)
library(OpenSourceAP.DownloadR)

test_that("dl_all_signals works in mock mode", {
  obj <- OpenAP$new(mock = TRUE)

  data <- obj$dl_all_signals()

  expect_true(is.data.frame(data))
  expect_true(all(c("permno", "yyyymm") %in% names(data)))
  expect_gt(nrow(data), 0)
})

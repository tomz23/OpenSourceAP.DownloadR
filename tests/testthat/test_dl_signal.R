library(testthat)
library(OpenSourceAP.DownloadR)


test_that("dl_signal_doc works in mock mode", {
  obj <- OpenAP$new(mock = TRUE)
  df <- obj$dl_signal_doc()

  expect_true(is.data.frame(df))
  expect_gt(nrow(df), 0)
})


test_that("dl_signal errors when no predictor is provided", {
  obj <- OpenAP$new(mock = TRUE)

  expect_error(obj$dl_signal(NULL))
  expect_error(obj$dl_signal(character(0)))
})

test_that("dl_signal works for a single OpenAP signal in mock mode", {
  obj <- OpenAP$new(mock = TRUE)

  data <- obj$dl_signal("BM")

  expect_true(is.data.frame(data))
  expect_true("BM" %in% colnames(data))
  expect_gt(nrow(data), 0)
})

test_that("dl_signal works for multiple OpenAP signals in mock mode", {
  obj <- OpenAP$new(mock = TRUE)
  data <- obj$dl_signal(c("BM", "Accruals"))
  expect_true(is.data.frame(data))
  expect_true(all(c("BM", "Accruals") %in% names(data)))
  expect_gt(nrow(data), 0)
})




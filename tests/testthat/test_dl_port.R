library(testthat)
library(OpenSourceAP.DownloadR)

test_that("dl_port works in mock mode for a single portfolio", {
  obj <- OpenAP$new(mock = TRUE)
  data <- obj$dl_port("mock_port")

  expect_true(is.data.frame(data))
  expect_true(all(c("signalname", "port", "date", "ret") %in% names(data)))
  expect_gt(nrow(data), 0)
})


test_that("dl_port filters predictors in mock mode", {
  obj <- OpenAP$new(mock = TRUE)
  data <- obj$dl_port("mock_port", predictor = "BM")

  expect_true("signalname" %in% names(data))
  expect_true(all(data[["signalname"]] == "BM"))
})



test_that("dl_port handles multiple predictors", {
  obj <- OpenAP$new(mock = TRUE)
  data <- obj$dl_port("mock_port", predictor = c("BM", "Accruals"))

  expect_true("signalname" %in% names(data))
  expect_true(all(data[["signalname"]] %in% c("BM", "Accruals")))
})




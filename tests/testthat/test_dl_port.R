library(testthat)
testthat::skip_if_not_installed("httptest")
library(httptest)
library(OpenSourceAP.DownloadR)

test_that("dl_port loads single portfolio correctly", {
  with_mock_api({
    obj <- OpenAP$new()
    data <- obj$dl_port("op")
    expect_true(is.data.frame(data))
  })
})


test_that("dl_port loads correctly with specific predictor", {
  with_mock_api({
    obj <- OpenAP$new()
    data <- obj$dl_port("deciles_vw", "AM")
    expect_true(is.data.frame(data))
  })
})


test_that("dl_port loads correctly with specific predictors", {
  with_mock_api({
    obj <- OpenAP$new()
    data <- obj$dl_port("deciles_vw", c("AM", "Mom12m"))
    expect_true(is.data.frame(data))
  })
})




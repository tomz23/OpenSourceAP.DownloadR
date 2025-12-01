library(testthat)
skip_if_not_installed("httptest")
library(httptest)
library(OpenSourceAP.DownloadR)

test_that("dl_signal works for multiple OpenAP signals", {
  with_mock_dir("folders", {
    obj <- OpenAP$new()

    data <- obj$dl_all_signals()

    expect_true(is.data.frame(data))
  })
})

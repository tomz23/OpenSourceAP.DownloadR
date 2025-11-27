library(testthat)
library(httptest)
library(OpenSourceAP.DownloadR)

test_that("dl_signal works for multiple OpenAP signals", {
  with_mock_api({
    obj <- OpenAP$new()

    data <- obj$dl_all_signals()

    expect_true(is.data.frame(data))
  })
})

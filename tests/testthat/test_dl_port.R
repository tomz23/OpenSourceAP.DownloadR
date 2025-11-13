library(testthat)
library(httptest)
library(OpenSourceAP.DownloadR)

test_that("dl_port behaves correctly", {
  with_mock_api({
    obj <- OpenAP$new()
    data <- obj$dl_port("op")
    expect_true(is.data.frame(data))
  })
})

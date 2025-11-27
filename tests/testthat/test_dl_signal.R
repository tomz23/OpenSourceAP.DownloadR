library(testthat)
library(httptest)
library(OpenSourceAP.DownloadR)


test_that("dl_signal_doc works", {
  with_mock_api({
    obj <- OpenAP$new()
    signal_doc <- obj$dl_signal_doc()

    expect_true(is.data.frame(signal_doc))
  })
})

test_that("dl_signal errors when no predictor is provided", {
  with_mock_api({
    obj <- OpenAP$new()

    expect_error(obj$dl_signal(NULL))
    expect_error(obj$dl_signal(character(0)))
  })
})

test_that("dl_signal works for a single OpenAP signal", {
  with_mock_api({
    obj <- OpenAP$new()

    data <- obj$dl_signal("BM")

    expect_true(is.data.frame(data))
    expect_true("BM" %in% colnames(data))
    expect_true(nrow(data) > 0)
  })
})

test_that("dl_signal works for multiple OpenAP signals", {
  with_mock_api({
    obj <- OpenAP$new()

    data <- obj$dl_signal(c("BM", "Accruals"))

    expect_true(is.data.frame(data))
    expect_true(all(c("BM", "Accruals") %in% colnames(data)))
    expect_true(nrow(data) > 0)
  })
})



# signals with WRDS connection cannot be tested properly


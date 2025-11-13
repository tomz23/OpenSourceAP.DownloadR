library(testthat)
library(httptest)

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

test_that("dl_signal errors when no predictor is provided", {
  with_mock_api({
    obj <- OpenAP$new()

    expect_error(obj$dl_signal(NULL))
    expect_error(obj$dl_signal(character(0)))
  })
})

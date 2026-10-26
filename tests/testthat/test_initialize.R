library(testthat)


test_that("OpenAP initializes correctly in mock mode", {

  obj <- OpenAP$new(mock = TRUE)

  expect_s3_class(obj, "OpenAP")
  expect_identical(obj$url, "mock")

  # Name maps should exist but be empty
  expect_true(is.data.frame(obj$name_id_map))
  expect_equal(nrow(obj$name_id_map), 0)

  expect_true(is.data.frame(obj$individual_signal_id_map))
  expect_equal(nrow(obj$individual_signal_id_map), 0)

  expect_true(is.data.frame(obj$signal_sign))
})


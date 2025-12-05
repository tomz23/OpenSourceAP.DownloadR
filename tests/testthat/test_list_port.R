library(testthat)

test_that("list_port works in mock mode", {
  obj <- OpenAP$new(mock = TRUE)

  result <- obj$list_port()

  expect_true(is.data.frame(result))
  expect_true(all(c("name", "download_name") %in% names(result)))
  expect_gt(nrow(result), 0)
})


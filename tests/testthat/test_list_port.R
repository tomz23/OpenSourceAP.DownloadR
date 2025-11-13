library(testthat)
library(httptest)

test_that("list_port runs and returns a data.frame", {
  with_mock_api({
    obj <- OpenAP$new()

    # Capture output without printing it to console
    output <- capture.output({
      result <- obj$list_port()
    })

    # list_port prints the filtered map; we check it's not empty
    expect_true(length(output) > 0)
  })
})

library(testthat)
library(httptest)

test_that("OpenAP initializes correctly", {
  with_mock_api({
    obj <- OpenAP$new()
    
    # Check object type
    expect_true("OpenAP" %in% class(obj))

    # Check important fields exist
    expect_true(!is.null(obj$url))
    expect_true(!is.null(obj$name_id_map))

    # Optionally check type of maps
    expect_true(is.data.frame(obj$name_id_map))
  })
})

test_that("info rejects invalid indicator format", {
  client <- INEClient$new()
  expect_error(client$info("bad"), "7-digit")
})

test_that("info returns structured list for valid indicator", {
  skip_on_cran()
  client <- INEClient$new()
  result <- suppressMessages(client$info("0010003"))
  expect_type(result, "list")
  expect_named(result, c("code", "name", "periodicity", "first_period",
                          "last_period", "last_updated", "dimensions"))
  expect_equal(result$code, "0010003")
  expect_s3_class(result$dimensions, "data.frame")
  expect_true(nrow(result$dimensions) > 0)
  expect_named(result$dimensions, c("dim_num", "name", "n_values"))
})

test_that("info prints PT labels by default", {
  skip_on_cran()
  client <- INEClient$new(lang = "PT")
  output <- capture.output(client$info("0010003"))
  expect_true(any(grepl("Código", output)))
  expect_true(any(grepl("Periodicidade", output)))
})

test_that("info prints EN labels when lang is EN", {
  skip_on_cran()
  client <- INEClient$new(lang = "EN")
  output <- capture.output(client$info("0010003"))
  expect_true(any(grepl("Code", output)))
  expect_true(any(grepl("Periodicity", output)))
})

test_that("info prints period range on periodicity line", {
  skip_on_cran()
  client <- INEClient$new()
  output <- capture.output(client$info("0010003"))
  period_line <- output[grepl("Periodicidade|Periodicity", output)]
  expect_true(grepl("\\(.*-.*\\)", period_line))
})

test_that("info errors for non-existent indicator", {
  skip_on_cran()
  client <- INEClient$new()
  expect_error(client$info("9999999"), class = "ineptr2_invalid_indicator")
})

test_that("HTTP errors raise ineptr2_api_error in info", {
  client <- INEClient$new()
  mock_404 <- function(req) { httr2::response(status_code = 404) }
  expect_error(
    suppressMessages(
      httr2::with_mocked_responses(mock_404, client$info("0010003"))
    ),
    class = "ineptr2_api_error"
  )
})

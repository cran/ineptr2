test_that("Invalid indicator format is rejected", {
  client <- INEClient$new()
  expect_error(client$get_data("invalid_id"), "7-digit")
  expect_error(client$get_data(123), "7-digit")
})

test_that("Indicator exists and call succeded", {
  skip_on_cran()
  client <- INEClient$new()
  expect_type(client$get_data("0008206", dim1 = "S7A1996", dim2 = c("11", "111"), dim4 = c(1, 19), dim5 = "TLES"), "list")
  expect_s3_class(client$get_data("0011823"), "data.frame")
  expect_named(client$get_data("0011823"))
  expect_true(all(c('dim_1', 'geocod', 'geodsg', 'valor') %in% names(client$get_data("0011823"))))
})

test_that("HTTP errors raise ineptr2_api_error", {
  client <- INEClient$new()
  mock_404 <- function(req) {httr2::response(status_code = 404)}
  mock_503 <- function(req) {httr2::response(status_code = 503)}
  expect_error(
    suppressMessages(httr2::with_mocked_responses(mock_404, client$get_data("0011823"))),
    class = "ineptr2_api_error"
  )
  expect_error(
    suppressMessages(httr2::with_mocked_responses(mock_503, client$get_data("0011823"))),
    class = "ineptr2_api_error"
  )
})

test_that("User input is correct", {
  skip_on_cran()
  client <- INEClient$new()
  expect_error(client$get_data("0010003", Blah = c("1234")))
  expect_error(client$get_data("0010003", dim1 = c("S7A2011"), dim2 = "PT", dim3 = "TLES"))
})

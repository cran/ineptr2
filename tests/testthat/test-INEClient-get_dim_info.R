test_that("Invalid indicator format is rejected", {
  client <- INEClient$new()
  expect_error(client$get_dim_info("invalid_id"), "7-digit")
  expect_error(client$get_dim_info(123), "7-digit")
})

test_that("Indicator exists and call succeded", {
  skip_on_cran()
  client <- INEClient$new()
  suppressMessages({
    expect_type(client$get_dim_info("0011823"), "list")
    expect_s3_class(client$get_dim_info("0011823"), "data.frame")
    expect_true(all(c('dim_num', 'abrv', 'versao') %in% names(client$get_dim_info("0011823"))))
  })
})

test_that("HTTP errors raise ineptr2_api_error", {
  client <- INEClient$new()
  mock_404 <- function(req) {httr2::response(status_code = 404)}
  mock_503 <- function(req) {httr2::response(status_code = 503)}
  expect_error(
    suppressMessages(httr2::with_mocked_responses(mock_404, client$get_dim_info("0011823"))),
    class = "ineptr2_api_error"
  )
  expect_error(
    suppressMessages(httr2::with_mocked_responses(mock_503, client$get_dim_info("0011823"))),
    class = "ineptr2_api_error"
  )
})

test_that("is_valid rejects invalid indicator format", {
  client <- INEClient$new()
  expect_error(client$is_valid("bad"), "7-digit")
  expect_error(client$is_valid(123), "7-digit")
})

test_that("is_valid returns TRUE for existing indicator", {
  skip_on_cran()
  client <- INEClient$new()
  expect_true(client$is_valid("0010003"))
})

test_that("is_valid returns FALSE for non-existent indicator", {
  skip_on_cran()
  client <- INEClient$new()
  expect_false(client$is_valid("9999999"))
})

test_that("HTTP errors raise ineptr2_api_error in is_valid", {
  client <- INEClient$new()
  mock_404 <- function(req) { httr2::response(status_code = 404) }
  expect_error(
    suppressMessages(
      httr2::with_mocked_responses(mock_404, client$is_valid("0010003"))
    ),
    class = "ineptr2_api_error"
  )
})

test_that("preview_chunks rejects invalid indicator format", {
  client <- INEClient$new()
  expect_error(client$preview_chunks("bad"), "7-digit")
})

test_that("preview_chunks returns chunk count and estimated rows", {
  skip_on_cran()
  client <- INEClient$new()
  result <- suppressMessages(client$preview_chunks("0010003"))
  expect_type(result, "list")
  expect_named(result, c("chunks", "estimated_rows"))
  expect_true(result$chunks >= 1)
  expect_true(result$estimated_rows >= 1)
})

test_that("preview_chunks prints informative message", {
  skip_on_cran()
  client <- INEClient$new()
  expect_message(client$preview_chunks("0010003"), "chunk")
  expect_message(client$preview_chunks("0010003"), "rows")
})

test_that("preview_chunks respects dimension filters", {
  skip_on_cran()
  client <- INEClient$new()
  full <- suppressMessages(client$preview_chunks("0010003"))
  filtered <- suppressMessages(client$preview_chunks("0010003", dim1 = "S7A2022"))
  expect_true(filtered$estimated_rows <= full$estimated_rows)
})

test_that("preview_chunks respects custom row_limit", {
  skip_on_cran()
  client <- INEClient$new()
  big <- suppressMessages(client$preview_chunks("0008273", row_limit = 1000000))
  small <- suppressMessages(client$preview_chunks("0008273", row_limit = 100))
  expect_true(small$chunks >= big$chunks)
  expect_equal(small$estimated_rows, big$estimated_rows)
})

test_that("preview_chunks errors for non-existent indicator", {
  skip_on_cran()
  client <- INEClient$new()
  expect_error(client$preview_chunks("9999999"), class = "ineptr2_invalid_indicator")
})

test_that("HTTP errors raise ineptr2_api_error in preview_chunks", {
  client <- INEClient$new()
  mock_404 <- function(req) { httr2::response(status_code = 404) }
  expect_error(
    suppressMessages(
      httr2::with_mocked_responses(mock_404, client$preview_chunks("0010003"))
    ),
    class = "ineptr2_api_error"
  )
})

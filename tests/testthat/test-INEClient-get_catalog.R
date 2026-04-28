test_that("get_catalog returns data frame with cached XML", {
  skip_on_cran()
  fixture <- get_catalog_fixture()

  catalog <- suppressMessages(fixture$client$get_catalog())
  expect_s3_class(catalog, "data.frame")
  expect_true(nrow(catalog) > 0)
  expect_true("varcd" %in% names(catalog))
  expect_true("title" %in% names(catalog))
})

test_that("Invalid lang is rejected at construction", {
  expect_error(INEClient$new(lang = "FR"))
})

test_that("HTTP errors raise ineptr2_api_error in get_catalog", {
  client <- INEClient$new()
  mock_404 <- function(req) {httr2::response(status_code = 404)}
  mock_503 <- function(req) {httr2::response(status_code = 503)}
  expect_error(
    suppressMessages(httr2::with_mocked_responses(mock_404, client$get_catalog())),
    class = "ineptr2_api_error"
  )
  expect_error(
    suppressMessages(httr2::with_mocked_responses(mock_503, client$get_catalog())),
    class = "ineptr2_api_error"
  )
})

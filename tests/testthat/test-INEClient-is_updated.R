test_that("Invalid indicator format is rejected", {
  client <- INEClient$new()
  expect_error(client$is_updated("invalid_id"), "7-digit")
})

test_that("is_updated errors for unparseable date", {
  client <- INEClient$new()
  expect_error(
    client$is_updated("0011823", last_updated = "not-a-date"),
    "Could not parse"
  )
})

test_that("is_updated errors when metadata lacks date field", {
  client <- INEClient$new()
  expect_error(
    client$is_updated("0011823", metadata = list()),
    "does not contain"
  )
})

test_that("is_updated errors when no reference available", {
  cache_dir <- tempfile("ineptr2_test_")
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  expect_error(
    client$is_updated("0011823"),
    "No reference date"
  )
})

test_that("is_updated detects update with old date", {
  skip_on_cran()
  client <- INEClient$new()
  expect_true(client$is_updated("0011823", last_updated = "2000-01-01"))
})

test_that("is_updated detects no update with future date", {
  skip_on_cran()
  client <- INEClient$new()
  expect_false(client$is_updated("0011823", last_updated = "2099-12-31"))
})

test_that("is_updated errors when cached metadata lacks DataUltimaAtualizacao", {
  cache_dir <- tempfile("ineptr2_test_")
  dir.create(cache_dir, recursive = TRUE)
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  meta_path <- file.path(cache_dir, "ine_0011823_PT_meta.json")
  jsonlite::write_json(
    list(Sucesso = list(Verdadeiro = "OK"), IndicadorCod = "0011823"),
    meta_path, auto_unbox = TRUE
  )

  expect_error(
    client$is_updated("0011823"),
    "Cached metadata does not contain"
  )
})

test_that("HTTP errors raise ineptr2_api_error", {
  client <- INEClient$new()
  mock_404 <- function(req) {httr2::response(status_code = 404)}
  expect_error(
    suppressMessages(httr2::with_mocked_responses(mock_404,
      client$is_updated("0011823", last_updated = "2000-01-01"))),
    class = "ineptr2_api_error"
  )
})

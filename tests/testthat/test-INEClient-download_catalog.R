test_that("download_catalog saves to cache and returns path", {
  skip_on_cran()
  fixture <- get_catalog_fixture()

  expect_type(fixture$path, "character")
  expect_true(file.exists(fixture$path))
  expect_true(file.info(fixture$path)$size > 0)
})

test_that("download_catalog uses cached file on second call", {
  skip_on_cran()
  fixture <- get_catalog_fixture()

  expect_message(fixture$client$download_catalog(), "Using cached catalog XML")
})

test_that("download_catalog enables caching even when use_cache is FALSE", {
  skip_on_cran()
  skip_catalog()
  cache_dir <- tempfile("ineptr2_test_")
  client <- INEClient$new(use_cache = FALSE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  xml_path <- file.path(cache_dir, paste0("ine_catalog_", client$lang, ".xml"))
  dir.create(cache_dir, recursive = TRUE)
  file.copy(get_catalog_fixture()$path, xml_path)

  path <- suppressMessages(client$download_catalog())

  expect_type(path, "character")
  expect_true(file.exists(path))
  expect_false(client$use_cache)
})

test_that("HTTP errors raise ineptr2_api_error in download_catalog", {
  client <- INEClient$new()
  mock_404 <- function(req) {httr2::response(status_code = 404)}
  expect_error(
    suppressMessages(
      httr2::with_mocked_responses(mock_404, client$download_catalog())
    ),
    class = "ineptr2_api_error"
  )
})

test_that("load_raw_data errors when no cache exists", {
  cache_dir <- tempfile("ineptr2_test_")
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  expect_error(
    client$load_raw_data("0011823"),
    "No cached data found"
  )
})

test_that("load_raw_data returns data after download_data", {
  skip_on_cran()
  cache_dir <- tempfile("ineptr2_test_")
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  suppressMessages(client$download_data("0011823"))

  raw <- client$load_raw_data("0011823")
  expect_type(raw, "list")
  expect_named(raw, c("responses", "urls"))
  expect_true(length(raw$responses) >= 1)
  expect_true(length(raw$urls) >= 1)
})

test_that("load_raw_data errors for incomplete downloads", {
  cache_dir <- tempfile("ineptr2_test_")
  dir.create(cache_dir, recursive = TRUE)
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  manifest_path <- file.path(cache_dir, "ine_0011823_PT_manifest.json")
  jsonlite::write_json(
    list(
      indicator = "0011823",
      lang = "PT",
      total_chunks = 3L,
      urls = c("url1", "url2", "url3"),
      complete = FALSE
    ),
    manifest_path, auto_unbox = TRUE
  )

  expect_error(
    client$load_raw_data("0011823"),
    "Download incomplete"
  )
})

test_that("load_raw_data output can be processed with process_ine_data", {
  skip_on_cran()
  cache_dir <- tempfile("ineptr2_test_")
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  suppressMessages(client$download_data("0011823"))

  raw <- client$load_raw_data("0011823")
  processed <- ineptr2:::process_ine_data(raw)
  expect_s3_class(processed, "data.frame")
  expect_true(nrow(processed) > 0)
})

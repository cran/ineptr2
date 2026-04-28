test_that("get_metadata uses cache on second call", {
  skip_on_cran()
  cache_dir <- tempfile("ineptr2_test_")
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  suppressMessages(client$get_metadata("0010003"))
  expect_message(client$get_metadata("0010003"), "Using cached metadata")
})

test_that("get_data uses cached processed data on second call", {
  skip_on_cran()
  cache_dir <- tempfile("ineptr2_test_")
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  suppressMessages(client$get_data("0011823"))
  expect_message(client$get_data("0011823"), "Using cached processed data")
})

test_that("get_data cache hit returns same data as original", {
  skip_on_cran()
  cache_dir <- tempfile("ineptr2_test_")
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  first <- suppressMessages(client$get_data("0011823"))
  second <- suppressMessages(client$get_data("0011823"))
  expect_equal(first, second)
})

test_that("get_data serves filtered subset from broader cache", {
  skip_on_cran()
  cache_dir <- tempfile("ineptr2_test_")
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  full <- suppressMessages(client$get_data("0010003"))
  filtered <- suppressMessages(client$get_data("0010003", dim1 = "S7A2022"))
  expect_true(nrow(filtered) <= nrow(full))
  expect_true(nrow(filtered) > 0)
})

test_that("clear_cache invalidates data cache", {
  skip_on_cran()
  cache_dir <- tempfile("ineptr2_test_")
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  suppressMessages(client$get_metadata("0010003"))
  suppressMessages(client$clear_cache("0010003"))

  cache_files <- list.files(cache_dir, pattern = "0010003")
  expect_equal(length(cache_files), 0)
})

test_that("caching does not interfere when use_cache is FALSE", {
  client <- INEClient$new(use_cache = FALSE)
  cache_dir_path <- tools::R_user_dir("ineptr2", "cache")
  files_before <- if (dir.exists(cache_dir_path)) list.files(cache_dir_path) else character(0)

  skip_on_cran()
  suppressMessages(client$get_metadata("0010003"))

  files_after <- if (dir.exists(cache_dir_path)) list.files(cache_dir_path) else character(0)
  expect_equal(length(files_after), length(files_before))
})

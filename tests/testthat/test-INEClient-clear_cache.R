test_that("clear_cache returns FALSE when no cache directory exists", {
  cache_dir <- tempfile("ineptr2_test_")
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)

  expect_message(result <- client$clear_cache(), "No cache directory found")
  expect_false(result)
})

test_that("clear_cache removes all cached files", {
  cache_dir <- tempfile("ineptr2_test_")
  dir.create(cache_dir, recursive = TRUE)
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)

  writeLines("{}", file.path(cache_dir, "ine_0011823_PT_meta.json"))
  writeLines("{}", file.path(cache_dir, "ine_0010003_PT_meta.json"))

  expect_message(result <- client$clear_cache(), "Cache cleared")
  expect_true(result)
  expect_false(dir.exists(cache_dir))
})

test_that("clear_cache removes specific indicator", {
  cache_dir <- tempfile("ineptr2_test_")
  dir.create(cache_dir, recursive = TRUE)
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  writeLines("{}", file.path(cache_dir, "ine_0011823_PT_meta.json"))
  writeLines("{}", file.path(cache_dir, "ine_0010003_PT_meta.json"))

  expect_message(result <- client$clear_cache("0011823"), "Cache cleared for indicator")
  expect_true(result)
  expect_false(file.exists(file.path(cache_dir, "ine_0011823_PT_meta.json")))
  expect_true(file.exists(file.path(cache_dir, "ine_0010003_PT_meta.json")))
})

test_that("clear_cache errors on invalid indicator format", {
  client <- INEClient$new()
  expect_error(client$clear_cache("bad"), "7-digit")
})

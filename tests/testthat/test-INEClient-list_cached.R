test_that("list_cached returns empty data frame when no cache directory exists", {
  cache_dir <- tempfile("ineptr2_test_")
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)

  expect_message(result <- client$list_cached(), "No cache directory found")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("list_cached returns empty data frame when cache is empty", {
  cache_dir <- tempfile("ineptr2_test_")
  dir.create(cache_dir, recursive = TRUE)
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  expect_message(result <- client$list_cached(), "No cached indicators found")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("list_cached detects metadata-only cache", {
  cache_dir <- tempfile("ineptr2_test_")
  dir.create(cache_dir, recursive = TRUE)
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  writeLines("{}", file.path(cache_dir, "ine_0011823_PT_meta.json"))

  result <- client$list_cached()
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$indicator, "0011823")
  expect_true(result$has_metadata)
  expect_false(result$has_data)
  expect_true(is.na(result$chunks_total))
  expect_false(result$download_complete)
})

test_that("list_cached reports complete download", {
  skip_on_cran()
  cache_dir <- tempfile("ineptr2_test_")
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  suppressMessages(client$download_data("0011823"))

  result <- client$list_cached()
  expect_s3_class(result, "data.frame")
  row <- result[result$indicator == "0011823", ]
  expect_true(row$has_metadata)
  expect_true(row$download_complete)
  expect_equal(row$chunks_downloaded, row$chunks_total)
})

test_that("list_cached reports incomplete download", {
  cache_dir <- tempfile("ineptr2_test_")
  dir.create(cache_dir, recursive = TRUE)
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  jsonlite::write_json(
    list(
      indicator = "0099999",
      lang = "PT",
      total_chunks = 5L,
      urls = as.list(paste0("url", 1:5)),
      complete = FALSE
    ),
    file.path(cache_dir, "ine_0099999_PT_manifest.json"),
    auto_unbox = TRUE
  )

  result <- client$list_cached()
  expect_equal(result$indicator, "0099999")
  expect_false(result$download_complete)
  expect_equal(result$chunks_downloaded, 0L)
  expect_equal(result$chunks_total, 5L)
})

test_that("list_cached respects language", {
  cache_dir <- tempfile("ineptr2_test_")
  dir.create(cache_dir, recursive = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  writeLines("{}", file.path(cache_dir, "ine_0011823_PT_meta.json"))
  writeLines("{}", file.path(cache_dir, "ine_0011823_EN_meta.json"))

  client_pt <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir, lang = "PT")
  client_en <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir, lang = "EN")

  result_pt <- client_pt$list_cached()
  result_en <- client_en$list_cached()

  expect_equal(nrow(result_pt), 1)
  expect_equal(nrow(result_en), 1)
})

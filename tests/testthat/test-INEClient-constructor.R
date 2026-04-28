test_that("Default constructor works", {
  client <- INEClient$new()
  expect_equal(client$lang, "PT")
  expect_false(client$use_cache)
  expect_null(client$cache_dir)
  expect_equal(client$row_limit, 1000000L)
  expect_equal(client$max_retries, 3L)
  expect_equal(client$progress_interval, 10L)
})

test_that("Custom constructor works", {
  client <- INEClient$new(lang = "EN", use_cache = TRUE, cache_dir = tempdir(),
                          row_limit = 10000L, max_retries = 5L,
                          progress_interval = 20L)
  expect_equal(client$lang, "EN")
  expect_true(client$use_cache)
  expect_equal(client$cache_dir, tempdir())
  expect_equal(client$row_limit, 10000L)
  expect_equal(client$max_retries, 5L)
  expect_equal(client$progress_interval, 20L)
})

test_that("Invalid lang is rejected", {
  expect_error(INEClient$new(lang = "FR"))
})

test_that("Active bindings validate on set", {
  client <- INEClient$new()
  expect_error(client$lang <- "FR")
  expect_error(client$use_cache <- "yes")
  expect_error(client$cache_dir <- 123)
  expect_error(client$row_limit <- -1)
  expect_error(client$row_limit <- "abc")
  expect_error(client$max_retries <- 0)
  expect_error(client$max_retries <- c(1, 2))
  expect_error(client$progress_interval <- -5)

  client$lang <- "EN"
  expect_equal(client$lang, "EN")

  client$use_cache <- TRUE
  expect_true(client$use_cache)

  client$cache_dir <- tempdir()
  expect_equal(client$cache_dir, tempdir())

  client$row_limit <- 25000
  expect_equal(client$row_limit, 25000L)

  client$max_retries <- 5
  expect_equal(client$max_retries, 5L)

  client$progress_interval <- 20
  expect_equal(client$progress_interval, 20L)
})

test_that("print shows client configuration", {
  client <- INEClient$new()
  output <- capture.output(print(client))
  expect_true(any(grepl("<INEClient>", output)))
  expect_true(any(grepl("Language.*PT", output)))
  expect_true(any(grepl("Cache.*disabled", output)))
  expect_true(any(grepl("Row limit.*1,000,000", output)))
})

test_that("print shows cache enabled with path", {
  client <- INEClient$new(use_cache = TRUE, cache_dir = tempdir())
  output <- capture.output(print(client))
  expect_true(any(grepl("Cache.*enabled", output)))
})

test_that("print reflects custom settings", {
  client <- INEClient$new(lang = "EN", max_retries = 7L, progress_interval = 50L,
                          timeout = 600)
  output <- capture.output(print(client))
  expect_true(any(grepl("Language.*EN", output)))
  expect_true(any(grepl("Max retries.*7", output)))
  expect_true(any(grepl("Progress interval.*50", output)))
  expect_true(any(grepl("Timeout.*600", output)))
})

test_that("print returns self invisibly", {
  client <- INEClient$new()
  result <- capture.output(ret <- print(client))
  expect_identical(ret, client)
})

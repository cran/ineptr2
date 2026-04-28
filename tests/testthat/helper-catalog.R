catalog_env <- new.env(parent = emptyenv())
catalog_env$cache_dir <- NULL
catalog_env$client <- NULL
catalog_env$path <- NULL

skip_catalog <- function() {
  if (!identical(Sys.getenv("INEPTR_TEST_CATALOG"), "true")) {
    testthat::skip("Catalog tests skipped. Set INEPTR_TEST_CATALOG=true to run.")
  }
}

get_catalog_fixture <- function() {
  skip_catalog()

  if (is.null(catalog_env$cache_dir)) {
    catalog_env$cache_dir <- tempfile("ineptr2_catalog_")
    catalog_env$client <- INEClient$new(use_cache = TRUE, cache_dir = catalog_env$cache_dir)
    catalog_env$path <- suppressMessages(catalog_env$client$download_catalog())
    withr::defer(unlink(catalog_env$cache_dir, recursive = TRUE),
                 envir = testthat::teardown_env())
  }

  catalog_env
}

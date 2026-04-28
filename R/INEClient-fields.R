#' INEClient configuration fields
#'
#' @description
#' Configuration fields for the [INEClient] class. All fields are
#' implemented as active bindings with validation. Set them with
#' `ine$field <- value` and read them with `ine$field`.
#'
#' @param lang Character. Language code: `"PT"` (default) or `"EN"`.
#'   Affects API responses, cache file paths, and display labels.
#' @param use_cache Logical. Whether to cache API responses locally.
#'   Default `FALSE`.
#' @param cache_dir Character or `NULL`. Cache directory path.
#'   If `NULL` (default), uses `tools::R_user_dir("ineptr2", "cache")`.
#' @param row_limit Integer. Maximum output rows per API request
#'   before splitting into chunks. Must be between 1 and 1 000 000
#'   (the API ceiling). Default `1000000L`.
#' @param max_retries Integer. Maximum retry attempts for failed
#'   chunk downloads. Default `3L`.
#' @param progress_interval Integer. Print a progress message every
#'   N chunks during downloads. Default `10L`.
#' @param timeout Numeric. Timeout in seconds for API requests
#'   (metadata and data endpoints). Default `300` (5 minutes).
#'   The catalog endpoint uses a separate, longer timeout.
#'
#' @examples
#' ine <- INEClient$new()
#' ine$lang
#' ine$lang <- "EN"
#'
#' ine$use_cache <- TRUE
#' ine$cache_dir <- tempdir()
#' ine$row_limit <- 500000L
#'
#' @seealso [INEClient] for methods.
#' @usage NULL
#' @name INEClient-fields
#' @rdname INEClient-fields
NULL

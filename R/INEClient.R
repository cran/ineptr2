#' @title INE API Client
#'
#' @description
#' An R6 class providing access to the Statistics Portugal (INE) API.
#' Holds configuration state (language, caching preferences) and provides
#' methods for retrieving data, metadata, and indicator catalog.
#'
#' See \code{\link{INEClient-fields}} for configurable fields (language,
#' caching, timeouts, etc.).
#'
#' @section Data:
#' \describe{
#'   \item{\code{get_data(indicator, row_limit, ...)}}{Retrieve tidy data
#'     for an indicator, with automatic chunking and optional caching.}
#'   \item{\code{download_data(indicator, row_limit, ...)}}{Download data to
#'     the file cache without loading into memory.}
#'   \item{\code{load_raw_data(indicator)}}{Load previously downloaded raw
#'     JSON data from the file cache.}
#'   \item{\code{preview_chunks(indicator, row_limit, ...)}}{Preview how many
#'     API chunks a download would require.}
#' }
#'
#' @section Metadata:
#' \describe{
#'   \item{\code{get_metadata(indicator)}}{Get cleaned metadata for an
#'     indicator.}
#'   \item{\code{info(indicator)}}{Print a summary of an indicator's key
#'     properties.}
#'   \item{\code{get_dim_info(indicator)}}{Get dimension descriptions.}
#'   \item{\code{get_dim_values(indicator, dims)}}{Get possible values for
#'     all dimensions.}
#'   \item{\code{is_valid(indicator)}}{Check if an indicator exists.}
#'   \item{\code{is_updated(indicator, last_updated, metadata)}}{Check if an
#'     indicator has been updated since last download.}
#' }
#'
#' @section Catalog:
#' \describe{
#'   \item{\code{get_catalog()}}{Download and parse the full indicator
#'     catalog (~10 min).}
#'   \item{\code{download_catalog()}}{Download the catalog to the file
#'     cache.}
#' }
#'
#' @section Cache:
#' \describe{
#'   \item{\code{list_cached()}}{List indicators present in the file cache.}
#'   \item{\code{clear_cache(indicator)}}{Clear cached files.}
#' }
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' # -- Setup --
#' ine <- INEClient$new()
#' ine <- INEClient$new(lang = "EN", use_cache = TRUE)
#' print(ine)
#'
#' # -- Metadata --
#' meta <- ine$get_metadata("0010003")
#' ine$info("0010003")
#' dims <- ine$get_dim_info("0010003")
#' vals <- ine$get_dim_values("0010003")
#'
#' # -- Data --
#' df <- ine$get_data("0010003")
#' df <- ine$get_data("0010003", dim1 = "S7A2024", dim2 = c("11", "17"))
#' ine$preview_chunks("0008273")
#'
#' # -- Validation --
#' ine$is_valid("0010003")
#' ine$is_updated("0010003", last_updated = "2024-01-01")
#'
#' # -- Cache --
#' ine$list_cached()
#' ine$clear_cache()
#'
#' @seealso \code{\link{INEClient-fields}} for field descriptions.
#' @importFrom R6 R6Class
#' @export
INEClient <- R6::R6Class(
  "INEClient",

  # ---------- PUBLIC METHODS ----------
  public = list(

    #' @description Create a new INE API client.
    #' @param lang Language code: `"PT"` (default) or `"EN"`.
    #' @param use_cache Logical. Whether to cache API responses. Default `FALSE`.
    #' @param cache_dir Character or `NULL`. Cache directory path.
    #'   If `NULL` (default), uses `tools::R_user_dir("ineptr2", "cache")`.
    #' @param row_limit Integer. Default maximum output rows per API request.
    #'   Default `1000000L`.
    #' @param max_retries Integer. Maximum retry attempts for failed chunk
    #'   downloads. Default `3L`.
    #' @param progress_interval Integer. Print a progress message every N chunks
    #'   during downloads. Default `10L`.
    #' @param timeout Numeric. Timeout in seconds for API requests (metadata
    #'   and data endpoints). Default `300` (5 minutes). The catalog endpoint
    #'   uses a separate, longer timeout.
    #' @return A new `INEClient` object.
    initialize = function(lang = "PT", use_cache = FALSE, cache_dir = NULL,
                          row_limit = 1000000L, max_retries = 3L,
                          progress_interval = 10L, timeout = 300) {
      self$lang <- lang
      self$use_cache <- use_cache
      self$cache_dir <- cache_dir
      self$row_limit <- row_limit
      self$max_retries <- max_retries
      self$progress_interval <- progress_interval
      self$timeout <- timeout
    },


    #' @description Retrieve tidy data for an indicator.
    #' @param indicator INE indicator ID as a 7-digit string. Example: `"0010003"`.
    #' @param row_limit Integer or `NULL`. Maximum output rows per API request
    #'   before splitting into multiple calls. If `NULL` (default), uses the
    #'   client's `row_limit` field. See **Details**.
    #' @param ... Dimension filters. Each argument should be named `dimN`
    #'   (where N is the dimension number) with a character vector of values.
    #'   Omitted dimensions include all values.
    #' @details
    #' ## Row limit and chunking
    #' The INE API limits each request to **1 000 000 output rows**, counted
    #' as the product of unique values across all dimensions. When the
    #' estimated row count exceeds `row_limit`, the request is automatically
    #' split into smaller chunks by iterating over one or more dimensions.
    #'
    #' If requests are timing out, try lowering `row_limit` (or increasing
    #' the client's `timeout` field) to produce more, smaller chunks.
    #'
    #' ## Caching
    #' When `use_cache` is enabled, processed data is stored as an RDS file.
    #' Subsequent calls with the same or narrower dimension filters return
    #' the cached result without hitting the API. Changing filters to
    #' include values outside the cached set triggers a fresh download.
    #' @return A data frame with the indicator data.
    get_data = function(indicator, row_limit = NULL, ...) {
      private$validate_indicator_id(indicator)
      if (is.null(row_limit)) row_limit <- self$row_limit

      metadata <- private$validate_and_prepare_metadata(indicator)

      current_filters <- normalize_dim_filters(list(...))

      if (self$use_cache) {
        cached <- private$read_data_cache(indicator)
        if (!is.null(cached) && is_filter_subset(current_filters, cached$dim_filters)) {
          message("Using cached processed data")
          return(filter_cached_data(cached$data, current_filters, metadata$dim_values))
        }
      }

      raw_data <- private$fetch_data_raw(indicator, row_limit, metadata, ...)
      if (is.null(raw_data)) {
        rlang::abort(c(
          paste0("Failed to retrieve data for indicator '", indicator, "'."),
          i = "The API may be unavailable."
        ), class = "ineptr2_api_error")
      }

      ret_data <- process_ine_data(raw_data)

      if (self$use_cache && !is.null(ret_data)) {
        private$write_data_cache(ret_data, current_filters, indicator)
      }

      return(ret_data)
    },

    #' @description Download data for an indicator to the file cache without
    #'   loading it into memory. Caching is temporarily enabled for the
    #'   duration of the call regardless of the client's `use_cache` setting.
    #' @param indicator INE indicator ID as a 7-digit string. Example: `"0010003"`.
    #' @param row_limit Integer or `NULL`. Maximum output rows per API request
    #'   before splitting into multiple calls. If `NULL` (default), uses the
    #'   client's `row_limit` field.
    #' @param ... Dimension filters in the form `dimN = value`.
    #' @return Invisibly, a list with `indicator`, `cache_dir`, `total_chunks`,
    #'   and `complete`, or `invisible(NULL)` on partial download failure
    #'   (resume by calling again).
    download_data = function(indicator, row_limit = NULL, ...) {
      private$validate_indicator_id(indicator)
      if (is.null(row_limit)) row_limit <- self$row_limit

      metadata <- private$validate_and_prepare_metadata(indicator)

      saved_use_cache <- self$use_cache
      self$use_cache <- TRUE
      on.exit(self$use_cache <- saved_use_cache)

      manifest <- private$download_chunks(indicator, row_limit, metadata, ...)
      if (is.null(manifest)) return(invisible(NULL))

      cache_dir <- private$get_chunk_dir(indicator)
      message("Data downloaded to: ", cache_dir)
      invisible(list(
        indicator = indicator,
        cache_dir = cache_dir,
        total_chunks = manifest$total_chunks,
        complete = manifest$complete
      ))
    },

    #' @description Load previously downloaded raw data from the file cache
    #'   as a list of parsed JSON responses. Use `download_data()` first to
    #'   populate the cache.
    #' @param indicator INE indicator ID as a 7-digit string. Example: `"0010003"`.
    #' @return A list with `responses` (parsed JSON) and `urls`.
    load_raw_data = function(indicator) {
      private$validate_indicator_id(indicator)

      manifest <- private$read_manifest(indicator)
      if (is.null(manifest)) {
        rlang::abort(c(
          paste0("No cached data found for indicator '", indicator, "'."),
          i = "Use `download_data()` first."
        ), class = "ineptr2_error")
      }
      if (!isTRUE(manifest$complete)) {
        n_cached <- sum(vapply(seq_len(manifest$total_chunks), function(i) {
          private$is_chunk_valid(indicator, i)
        }, logical(1)))
        rlang::abort(c(
          paste0("Download incomplete for indicator '", indicator, "': ",
                 n_cached, "/", manifest$total_chunks, " chunks cached."),
          i = "Use `download_data()` to resume."
        ), class = "ineptr2_error")
      }
      return(private$assemble_chunks(indicator, manifest$urls))
    },

    #' @description Get cleaned metadata for an indicator.
    #' @param indicator INE indicator ID as a 7-digit string. Example: `"0010003"`.
    #' @return API response body as a list.
    get_metadata = function(indicator) {
      private$validate_indicator_id(indicator)

      temp_metadata <- private$get_metadata_raw(indicator)
      if (is.null(temp_metadata)) {
        rlang::abort(c(
          paste0("Failed to retrieve metadata for indicator '", indicator, "'."),
          i = "The API may be unavailable."
        ), class = "ineptr2_api_error")
      }
      temp_metadata["Dimensoes"] <- NULL
      temp_metadata["Sucesso"] <- NULL
      if (length(temp_metadata) == 0) {
        rlang::abort(private$msg_invalid_indicator(),
                     class = "ineptr2_invalid_indicator")
      }
      return(temp_metadata)
    },

    #' @description Get the full INE indicator catalog.
    #'   This operation is very time-consuming (~10 minutes) as it downloads
    #'   the entire catalog from the INE API. Consider using `download_catalog()`
    #'   to cache the result for subsequent calls.
    #' @return A data frame with one row per indicator.
    get_catalog = function() {
      xml_string <- private$fetch_catalog_raw()
      if (is.null(xml_string)) {
        rlang::abort(c(
          "Failed to download the INE catalog.",
          i = "The API may be unavailable."
        ), class = "ineptr2_api_error")
      }
      process_ine_catalog(xml_string)
    },

    #' @description Download the INE indicator catalog to the file cache
    #'   without loading it into memory. This operation is time-consuming
    #'   (~10 minutes) as it downloads the entire catalog from the INE API.
    #'   Subsequent calls return the cached file immediately. Caching is
    #'   temporarily enabled for the duration of the call regardless of
    #'   the client's `use_cache` setting.
    #' @return Invisibly, the cache file path.
    download_catalog = function() {
      cache_path <- private$get_catalog_cache_path()
      cache_dir <- dirname(cache_path)

      if (file.exists(cache_path) && file.info(cache_path)$size > 0) {
        message("Using cached catalog XML")
        message("Catalog downloaded to: ", cache_path)
        return(invisible(cache_path))
      }

      if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

      req <- httr2::request(private$.url_catalog) |>
        httr2::req_url_query(opc = 2, lang = self$lang) |>
        httr2::req_timeout(seconds = 1200) |>
        httr2::req_error(is_error = \(resp) FALSE)

      message("Downloading INE catalog. This may take several minutes...")
      resp <- gracefully_fail(req)
      if (is.null(resp)) {
        rlang::abort(c(
          "Failed to download the INE catalog.",
          i = "The API may be unavailable."
        ), class = "ineptr2_api_error")
      }

      xml_string <- httr2::resp_body_string(resp)
      tryCatch(
        writeLines(xml_string, cache_path),
        error = function(e) {
          message("Warning: could not write catalog cache: ", e$message)
        }
      )

      message("Catalog downloaded to: ", cache_path)
      invisible(cache_path)
    },

    #' @description Print a summary of an indicator's key properties:
    #'   code, name, periodicity and time range, last update date,
    #'   and a per-dimension breakdown of unique values.
    #'   Labels are displayed in the client's current language.
    #' @param indicator INE indicator ID as a 7-digit string. Example: `"0010003"`.
    #' @return Invisibly, a list with `code`, `name`, `periodicity`,
    #'   `first_period`, `last_period`, `last_updated`, and
    #'   `dimensions` (a data frame with `dim_num`, `name`, and
    #'   `n_values` columns).
    info = function(indicator) {
      private$validate_indicator_id(indicator)

      metadata_raw <- private$get_metadata_raw(indicator)
      if (is.null(metadata_raw)) {
        rlang::abort(c(
          paste0("Failed to retrieve metadata for indicator '", indicator, "'."),
          i = "The API may be unavailable."
        ), class = "ineptr2_api_error")
      }
      if (is.null(metadata_raw$Sucesso$Verdadeiro)) {
        rlang::abort(private$msg_invalid_indicator(),
                     class = "ineptr2_invalid_indicator")
      }

      dim_desc <- records_to_df(metadata_raw[["Dimensoes"]][["Descricao_Dim"]])
      dims_length <- calc_dims_length_from_raw(metadata_raw)

      dimensions <- merge(dim_desc[, c("dim_num", "abrv")], dims_length,
                          by = "dim_num", sort = TRUE)
      names(dimensions) <- c("dim_num", "name", "n_values")

      result <- list(
        code         = metadata_raw$IndicadorCod,
        name         = metadata_raw$IndicadorNome,
        periodicity  = metadata_raw$Periodic,
        first_period = metadata_raw$PrimeiroPeriodo,
        last_period  = metadata_raw$UltimoPeriodo,
        last_updated = metadata_raw$DataUltimaAtualizacao,
        dimensions   = dimensions
      )

      labels <- if (self$lang == "PT") {
        list(code = "C\u00f3digo", name = "Nome", periodicity = "Periodicidade",
             updated = "\u00dalt. atualiza\u00e7\u00e3o",
             dims = "Dimens\u00f5es", values = "valores")
      } else {
        list(code = "Code", name = "Name", periodicity = "Periodicity",
             updated = "Last updated", dims = "Dimensions", values = "values")
      }

      period_str <- paste0(result$periodicity,
                           " (", result$first_period, " - ", result$last_period, ")")

      cat(
        "<INE Indicator>\n",
        "  ", labels$code, ": ", result$code, "\n",
        "  ", labels$name, ": ", result$name, "\n",
        "  ", labels$periodicity, ": ", period_str, "\n",
        "  ", labels$updated, ": ", result$last_updated, "\n",
        "  ", labels$dims, ": ", nrow(dimensions), "\n",
        sep = ""
      )
      for (i in seq_len(nrow(dimensions))) {
        cat(sprintf("    dim%s: %s (%d %s)\n",
                    dimensions$dim_num[i],
                    dimensions$name[i],
                    dimensions$n_values[i],
                    labels$values))
      }

      invisible(result)
    },

    #' @description Get dimension descriptions for an indicator.
    #' @param indicator INE indicator ID as a 7-digit string. Example: `"0010003"`.
    #' @return A data frame with dim_num, abrv, and versao columns.
    get_dim_info = function(indicator) {
      private$validate_indicator_id(indicator)

      metadata <- private$get_metadata_raw(indicator)
      if (is.null(metadata)) {
        rlang::abort(c(
          paste0("Failed to retrieve metadata for indicator '", indicator, "'."),
          i = "The API may be unavailable."
        ), class = "ineptr2_api_error")
      }
      if (is.null(metadata$Sucesso$Verdadeiro)) {
        rlang::abort(private$msg_invalid_indicator(),
                     class = "ineptr2_invalid_indicator")
      }

      records_to_df(metadata[["Dimensoes"]][["Descricao_Dim"]])
    },

    #' @description Get possible values for all dimensions of an indicator.
    #' @param indicator INE indicator ID as a 7-digit string. Example: `"0010003"`.
    #' @param dims Integer vector of dimension numbers to include,
    #'   or `NULL` (default) for all dimensions.
    #' @return A tidy data frame with dimension values.
    get_dim_values = function(indicator, dims = NULL) {
      private$validate_indicator_id(indicator)

      metadata <- private$get_metadata_raw(indicator)
      if (is.null(metadata)) {
        rlang::abort(c(
          paste0("Failed to retrieve metadata for indicator '", indicator, "'."),
          i = "The API may be unavailable."
        ), class = "ineptr2_api_error")
      }
      if (is.null(metadata$Sucesso$Verdadeiro)) {
        rlang::abort(private$msg_invalid_indicator(),
                     class = "ineptr2_invalid_indicator")
      }

      result <- extract_dim_values(metadata)

      if (!is.null(dims)) {
        result <- result[result$dim_num %in% as.character(dims), , drop = FALSE]
      }

      result
    },

    #' @description Preview how many API chunks a download would require,
    #'   without fetching any data. Useful for estimating download time
    #'   before committing to a large request.
    #' @param indicator INE indicator ID as a 7-digit string. Example: `"0010003"`.
    #' @param row_limit Integer or `NULL`. Maximum output rows per API request
    #'   before splitting into multiple calls. If `NULL` (default), uses the
    #'   client's `row_limit` field.
    #' @param ... Dimension filters in the form `dimN = value`.
    #' @return Invisibly, a list with `chunks` and `estimated_rows`.
    preview_chunks = function(indicator, row_limit = NULL, ...) {
      private$validate_indicator_id(indicator)
      if (is.null(row_limit)) row_limit <- self$row_limit

      metadata <- private$validate_and_prepare_metadata(indicator)
      urls <- private$get_api_urls(indicator, row_limit, metadata, ...)

      chunks <- length(urls)

      user_filters <- list(...)
      filter_names <- sub("^(\\w)", "\\U\\1", tolower(names(user_filters)), perl = TRUE)
      names(user_filters) <- filter_names
      user_dim_nums <- as.integer(gsub("\\D", "", filter_names))

      eff_lens <- metadata$dims_length
      for (i in seq_along(filter_names)) {
        d <- user_dim_nums[i]
        eff_lens$n[eff_lens$dim_num == d] <- length(user_filters[[filter_names[i]]])
      }
      estimated_rows <- prod(eff_lens$n)

      message(sprintf(
        "Indicator '%s' would require %s chunk%s (estimated %s rows)",
        indicator,
        formatC(chunks, format = "d", big.mark = ","),
        if (chunks == 1L) "" else "s",
        formatC(estimated_rows, format = "d", big.mark = ",")
      ))

      invisible(list(chunks = chunks, estimated_rows = estimated_rows))
    },

    #' @description Check if an indicator exists and is callable via the INE API.
    #' @param indicator INE indicator ID as a 7-digit string. Example: `"0010003"`.
    #' @return `TRUE` if indicator exists, `FALSE` otherwise.
    is_valid = function(indicator) {
      private$validate_indicator_id(indicator)

      metadata <- private$get_metadata_raw(indicator)
      if (is.null(metadata)) {
        rlang::abort(c(
          paste0("Failed to retrieve metadata for indicator '", indicator, "'."),
          i = "The API may be unavailable."
        ), class = "ineptr2_api_error")
      }
      return(!is.null(metadata$Sucesso$Verdadeiro))
    },

    #' @description Check if an indicator has been updated since last download.
    #' @param indicator INE indicator ID as a 7-digit string. Example: `"0010003"`.
    #' @param last_updated A `Date` object or a character string in `"YYYY-MM-DD"` format.
    #'   If provided, takes precedence over cached metadata. If `NULL` (default),
    #'   the function looks for cached metadata or the `metadata` argument.
    #' @param metadata A metadata list object as returned by `get_metadata()`.
    #'   If provided and `last_updated` is `NULL`, extracts `DataUltimaAtualizacao`.
    #' @return `TRUE` if updated, `FALSE` if not.
    is_updated = function(indicator, last_updated = NULL, metadata = NULL) {
      private$validate_indicator_id(indicator)

      ref_date <- NULL

      if (!is.null(last_updated)) {
        ref_date <- tryCatch(as.Date(last_updated), error = function(e) NULL)
        if (is.null(ref_date)) {
          rlang::abort("Could not parse `last_updated` as a date.",
                       class = "ineptr2_error")
        }
      } else if (!is.null(metadata)) {
        ref_date_str <- metadata$DataUltimaAtualizacao
        if (is.null(ref_date_str)) {
          rlang::abort("Provided metadata does not contain 'DataUltimaAtualizacao'.",
                       class = "ineptr2_error")
        }
        ref_date <- as.Date(ref_date_str)
      } else {
        cached <- private$read_metadata_cache(indicator)
        if (is.null(cached)) {
          rlang::abort(c(
            paste0("No reference date provided and no cached metadata found for indicator '",
                   indicator, "'."),
            i = "Provide `last_updated`, `metadata`, or enable caching first."
          ), class = "ineptr2_error")
        }
        ref_date_str <- cached$DataUltimaAtualizacao
        if (is.null(ref_date_str)) {
          rlang::abort("Cached metadata does not contain 'DataUltimaAtualizacao'.",
                       class = "ineptr2_error")
        }
        ref_date <- as.Date(ref_date_str)
      }

      # Fetch live metadata (bypass cache to get the latest)
      saved_use_cache <- self$use_cache
      self$use_cache <- FALSE
      live_meta <- private$get_metadata_raw(indicator)
      self$use_cache <- saved_use_cache

      if (is.null(live_meta)) {
        rlang::abort(c(
          paste0("Failed to retrieve live metadata for indicator '", indicator, "'."),
          i = "The API may be unavailable."
        ), class = "ineptr2_api_error")
      }

      if (is.null(live_meta$Sucesso$Verdadeiro)) {
        rlang::abort(private$msg_invalid_indicator(),
                     class = "ineptr2_invalid_indicator")
      }

      live_date_str <- live_meta$DataUltimaAtualizacao
      if (is.null(live_date_str)) {
        rlang::abort("Live metadata does not contain 'DataUltimaAtualizacao'.",
                     class = "ineptr2_api_error")
      }

      live_date <- as.Date(live_date_str)
      return(live_date > ref_date)
    },

    #' @description List indicators present in the file cache.
    #' @return A data frame with one row per cached indicator and columns
    #'   `indicator`, `has_metadata`, `has_data`, `chunks_downloaded`,
    #'   `chunks_total`, and `download_complete`. Returns a zero-row data
    #'   frame if no cache exists.
    list_cached = function() {
      empty <- data.frame(
        indicator = character(),
        has_metadata = logical(),
        has_data = logical(),
        chunks_downloaded = integer(),
        chunks_total = integer(),
        download_complete = logical(),
        stringsAsFactors = FALSE
      )

      dir <- private$get_cache_dir_path()
      if (!dir.exists(dir)) {
        message("No cache directory found at: ", dir)
        return(empty)
      }

      files <- list.files(dir, pattern = sprintf("^ine_\\d{7}_%s_", self$lang))
      ids <- unique(regmatches(files, regexpr("\\d{7}", files)))

      if (length(ids) == 0) {
        message("No cached indicators found")
        return(empty)
      }

      rows <- lapply(ids, function(id) {
        manifest <- private$read_manifest(id)
        chunks_total <- if (!is.null(manifest)) manifest$total_chunks else NA_integer_
        chunks_downloaded <- if (!is.null(manifest)) {
          sum(vapply(seq_len(manifest$total_chunks), function(i) {
            file.exists(private$get_chunk_path(id, i))
          }, logical(1)))
        } else {
          NA_integer_
        }

        data.frame(
          indicator         = id,
          has_metadata       = file.exists(private$get_metadata_cache_path(id)),
          has_data           = file.exists(private$get_data_cache_path(id)),
          chunks_downloaded  = chunks_downloaded,
          chunks_total       = chunks_total,
          download_complete  = isTRUE(manifest$complete),
          stringsAsFactors   = FALSE
        )
      })

      do.call(rbind, rows)
    },

    #' @description Clear cached files.
    #' @param indicator Optional INE indicator ID. If `NULL` (default), clears all cached files.
    #' @return Invisibly returns `TRUE` if files were removed, `FALSE` otherwise.
    clear_cache = function(indicator = NULL) {
      if (!is.null(indicator)) private$validate_indicator_id(indicator)

      dir <- private$get_cache_dir_path()

      if (!dir.exists(dir)) {
        message("No cache directory found at: ", dir)
        return(invisible(FALSE))
      }

      if (is.null(indicator)) {
        unlink(dir, recursive = TRUE)
        message("Cache cleared: ", dir)
        return(invisible(TRUE))
      }

      private$invalidate_chunk_cache(indicator)
      private$invalidate_metadata_cache(indicator)
      private$invalidate_data_cache(indicator)
      message("Cache cleared for indicator: ", indicator)
      return(invisible(TRUE))
    },

    #' @description Print a summary of the client configuration.
    #' @param ... Ignored.
    print = function(...) {
      cache_status <- if (self$use_cache) {
        paste0("enabled (", private$get_cache_dir_path(), ")")
      } else {
        "disabled"
      }
      cat(
        "<INEClient>\n",
        "  Language:          ", self$lang, "\n",
        "  Cache:             ", cache_status, "\n",
        "  Row limit:         ", formatC(self$row_limit, format = "d", big.mark = ","), "\n",
        "  Max retries:       ", self$max_retries, "\n",
        "  Progress interval: ", self$progress_interval, "\n",
        "  Timeout (s):       ", self$timeout, "\n",
        sep = ""
      )
      invisible(self)
    }
  ),

  # ---------- PRIVATE METHODS ----------
  private = list(

    # --- State ---
    .lang = NULL,
    .use_cache = NULL,
    .cache_dir = NULL,
    .row_limit = NULL,
    .max_retries = NULL,
    .progress_interval = NULL,
    .timeout = NULL,

    # --- API URL constants ---
    .url_metadata = "https://www.ine.pt/ine/json_indicador/pindicaMeta.jsp",
    .url_data = "https://www.ine.pt/ine/json_indicador/pindica.jsp",
    .url_catalog = "https://www.ine.pt/ine/xml_indic.jsp",

    validate_indicator_id = function(indicator) {
      if (!is.character(indicator) || length(indicator) != 1 || !grepl("^\\d{7}$", indicator)) {
        stop(sprintf(
          "Indicator must be a 7-digit string (e.g., '0010003'), got: %s",
          deparse(indicator)
        ), call. = FALSE)
      }
    },

    msg_invalid_indicator = function() {
      switch(self$lang,
        PT = "O c\u00F3digo do indicador n\u00E3o existe.",
        EN = "The indicator code does not exist."
      )
    },

    validate_and_prepare_metadata = function(indicator) {
      metadata_raw <- private$get_metadata_raw(indicator)
      if (is.null(metadata_raw)) {
        rlang::abort(c(
          paste0("Failed to retrieve metadata for indicator '", indicator, "'."),
          i = "The API may be unavailable."
        ), class = "ineptr2_api_error")
      }

      if (is.null(metadata_raw$Sucesso$Verdadeiro)) {
        rlang::abort(private$msg_invalid_indicator(),
                     class = "ineptr2_invalid_indicator")
      }

      list(
        raw = metadata_raw,
        num_dims = length(metadata_raw$Dimensoes[[1]]),
        dim_values = extract_dim_values(metadata_raw),
        dims_length = calc_dims_length_from_raw(metadata_raw)
      )
    },

    download_chunks = function(indicator, row_limit, metadata, ...) {
      myurls <- private$get_api_urls(indicator, row_limit, metadata, ...)
      if (is.null(myurls)) return(invisible(NULL))

      total <- length(myurls)
      manifest <- private$read_manifest(indicator)

      if (!is.null(manifest) && !identical(manifest$urls, as.character(myurls))) {
        message("Dimension filters changed. Clearing chunk cache.")
        private$invalidate_chunk_cache(indicator)
        manifest <- NULL
      }

      if (!is.null(manifest) && isTRUE(manifest$complete)) {
        message("All ", manifest$total_chunks, " chunks already cached")
        return(manifest)
      }

      if (is.null(manifest)) {
        chunk_dir <- private$get_chunk_dir(indicator)
        if (!dir.exists(chunk_dir)) dir.create(chunk_dir, recursive = TRUE)
        manifest <- list(
          indicator = indicator,
          lang = self$lang,
          total_chunks = total,
          urls = as.character(myurls),
          complete = FALSE
        )
        private$write_manifest(manifest, indicator)
      }

      pending <- which(!vapply(seq_len(total), function(i) {
        private$is_chunk_valid(indicator, i)
      }, logical(1)))

      if (length(pending) == 0) {
        manifest$complete <- TRUE
        private$write_manifest(manifest, indicator)
        message("All ", total, " chunks already cached")
        return(manifest)
      }

      already_done <- total - length(pending)
      if (already_done > 0) {
        message("Resuming download: ", already_done, "/", total, " chunks cached")
      }

      last_pending <- max(pending)

      for (i in pending) {
        success <- FALSE
        final_path <- private$get_chunk_path(indicator, i)
        temp_path <- paste0(final_path, ".part")

        for (attempt in seq_len(self$max_retries)) {
          req <- httr2::request(myurls[i]) |>
            httr2::req_timeout(seconds = self$timeout) |>
            httr2::req_error(is_error = \(resp) FALSE)
          resp <- gracefully_fail(req, path = temp_path)
          if (!is.null(resp) && file.exists(temp_path)) {
            if (finalize_chunk(temp_path, final_path)) {
              success <- TRUE
              break
            }
          }
          if (file.exists(temp_path)) unlink(temp_path)
        }

        if (!success) {
          done_so_far <- sum(vapply(seq_len(total), function(j) {
            file.exists(private$get_chunk_path(indicator, j))
          }, logical(1)))
          message("Download failed at chunk ", i, "/", total,
                  ". ", done_so_far, " chunks saved.",
                  " Resume by calling again.")
          return(invisible(NULL))
        }

        if (i %% self$progress_interval == 0 || i == last_pending) {
          message("Downloaded chunk ", i, "/", total)
        }
      }

      manifest$complete <- TRUE
      private$write_manifest(manifest, indicator)
      message("All ", total, " chunks downloaded and cached")
      return(manifest)
    },

    # --- Internal API methods ---

    get_metadata_raw = function(indicator) {
      if (self$use_cache) {
        cached_meta <- private$read_metadata_cache(indicator)
        if (!is.null(cached_meta)) {
          message("Using cached metadata")
          return(cached_meta)
        }
      }

      params <- list(
        varcd = indicator,
        lang = self$lang
      )

      req <- httr2::request(base_url = private$.url_metadata) |>
        httr2::req_url_query(!!!params) |>
        httr2::req_timeout(seconds = self$timeout) |>
        httr2::req_error(is_error = \(resp) FALSE)

      resp <- gracefully_fail(req)

      if (is.null(resp)) {
        return(invisible(NULL))
      }

      metadata_raw <- resp |>
        httr2::resp_body_json() |>
        {\(x) x[[1]]}()

      if (self$use_cache) {
        private$write_metadata_cache(metadata_raw, indicator)
      }

      return(metadata_raw)
    },

    fetch_data_raw = function(indicator, row_limit, metadata, ...) {
      if (self$use_cache) {
        manifest <- private$download_chunks(indicator, row_limit, metadata, ...)
        if (is.null(manifest)) return(invisible(NULL))
        return(private$assemble_chunks(indicator, manifest$urls))
      }

      myurls <- private$get_api_urls(indicator, row_limit, metadata, ...)
      if (is.null(myurls)) return(invisible(NULL))

      reqs <- lapply(myurls, \(x) httr2::request(x) |>
                     httr2::req_timeout(seconds = self$timeout) |>
                     httr2::req_error(is_error = \(resp) FALSE))
      responses <- lapply(reqs, gracefully_fail)

      if (any(vapply(responses, is.null, logical(1)))) {
        return(invisible(NULL))
      }

      response_bodies <- lapply(responses, httr2::resp_body_json)

      list(responses = response_bodies, urls = myurls)
    },

    get_api_urls = function(indicator, row_limit, metadata, ...) {
      user_filters <- list(...)
      filter_names <- sub("^(\\w)", "\\U\\1", tolower(names(user_filters)), perl = TRUE)
      names(user_filters) <- filter_names

      dims_len <- metadata$dims_length
      dim_values <- metadata$dim_values

      if (length(user_filters) > metadata$num_dims) {
        stop(sprintf("You are trying to extract more dimensions than are available for indicator: %s (%s passed, %s allowed)",
                     indicator, length(user_filters), metadata$num_dims))
      }

      if (length(filter_names) > 0) {
        bad <- !grepl("^dim\\d$", filter_names, ignore.case = TRUE)
        if (any(bad)) {
          stop(sprintf("All parameters should be in the form 'dimN'. Error at: %s",
                       paste(filter_names[bad], collapse = ", ")))
        }
      }

      user_dim_nums <- as.integer(gsub("\\D", "", filter_names))

      # Effective length per dimension: user-provided count or full metadata length
      eff_lens <- dims_len
      for (i in seq_along(filter_names)) {
        d <- user_dim_nums[i]
        eff_lens$n[eff_lens$dim_num == d] <- length(user_filters[[filter_names[i]]])
      }

      total_rows <- prod(eff_lens$n)

      # Find dimensions to split so each response stays within row_limit
      dims_to_split <- integer(0)
      remaining_rows <- total_rows
      available <- eff_lens

      while (remaining_rows > row_limit && nrow(available) > 0) {
        available$rows_after <- remaining_rows / available$n
        viable <- available[available$rows_after <= row_limit, , drop = FALSE]

        if (nrow(viable) > 0) {
          best <- viable[which.min(viable$n), , drop = FALSE]
        } else {
          best <- available[which.max(available$n), , drop = FALSE]
        }

        d <- as.integer(best$dim_num[1])
        dims_to_split <- c(dims_to_split, d)
        remaining_rows <- remaining_rows / best$n[1]
        available <- available[available$dim_num != d, , drop = FALSE]
      }

      # Build URL parameters
      url_params <- list(op = 2, varcd = indicator, lang = self$lang)

      for (d in sort(unique(as.integer(dims_len$dim_num)))) {
        dim_name <- paste0("Dim", d)
        is_user <- d %in% user_dim_nums
        is_split <- d %in% dims_to_split

        if (is_split) {
          if (is_user) {
            url_params[[dim_name]] <- user_filters[[dim_name]]
          } else {
            url_params[[dim_name]] <- dim_values$cat_id[dim_values$dim_num == d]
          }
        } else if (is_user) {
          url_params[[dim_name]] <- paste(user_filters[[dim_name]], collapse = ",")
        } else if (d == 1L) {
          url_params[[dim_name]] <- "T"
        }
      }

      param_grid <- expand.grid(url_params, stringsAsFactors = FALSE)
      urls <- vapply(seq_len(nrow(param_grid)), function(i) {
        params <- as.list(param_grid[i, ])
        req <- httr2::request(private$.url_data) |>
          httr2::req_url_query(!!!params)
        req[["url"]]
      }, character(1))

      return(urls)
    },

    fetch_catalog_raw = function() {
      if (self$use_cache) {
        cache_path <- private$get_catalog_cache_path()
        if (file.exists(cache_path) && file.info(cache_path)$size > 0) {
          xml_string <- tryCatch(
            readLines(cache_path, warn = FALSE) |> paste(collapse = "\n"),
            error = function(e) NULL
          )
          if (!is.null(xml_string) && nchar(xml_string) > 0) {
            message("Using cached catalog XML")
            return(xml_string)
          }
        }
      }

      req <- httr2::request(private$.url_catalog) |>
        httr2::req_url_query(opc = 2, lang = self$lang) |>
        httr2::req_timeout(seconds = 1200) |>
        httr2::req_error(is_error = \(resp) FALSE)

      message("Downloading INE catalog. This may take several minutes...")
      resp <- gracefully_fail(req)
      if (is.null(resp)) return(invisible(NULL))

      xml_string <- httr2::resp_body_string(resp)

      if (self$use_cache) {
        cache_dir <- private$get_cache_dir_path()
        if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
        tryCatch(
          writeLines(xml_string, private$get_catalog_cache_path()),
          error = function(e) message("Warning: could not write catalog cache: ", e$message)
        )
      }

      xml_string
    },

    # --- Cache methods ---

    get_cache_dir_path = function() {
      path <- if (is.null(self$cache_dir)) {
        tools::R_user_dir("ineptr2", "cache")
      } else {
        self$cache_dir
      }
      normalizePath(path, winslash = "/", mustWork = FALSE)
    },

    get_chunk_dir = function(indicator) {
      dir <- private$get_cache_dir_path()
      file.path(dir, sprintf("ine_%s_%s_chunks", indicator, self$lang))
    },

    get_chunk_path = function(indicator, index) {
      chunk_dir <- private$get_chunk_dir(indicator)
      file.path(chunk_dir, sprintf("chunk_%04d.json", index))
    },

    get_manifest_path = function(indicator) {
      dir <- private$get_cache_dir_path()
      file.path(dir, sprintf("ine_%s_%s_manifest.json", indicator, self$lang))
    },

    read_manifest = function(indicator) {
      path <- private$get_manifest_path(indicator)
      if (!file.exists(path)) return(NULL)
      tryCatch(
        jsonlite::fromJSON(path, simplifyVector = TRUE),
        error = function(e) NULL
      )
    },

    write_manifest = function(manifest, indicator) {
      path <- private$get_manifest_path(indicator)
      dir <- dirname(path)
      if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
      jsonlite::write_json(manifest, path, auto_unbox = TRUE, pretty = TRUE)
    },

    is_chunk_valid = function(indicator, index) {
      path <- private$get_chunk_path(indicator, index)
      if (!file.exists(path)) return(FALSE)
      if (file.info(path)$size == 0) return(FALSE)
      tryCatch({
        jsonlite::fromJSON(path, simplifyVector = FALSE)
        TRUE
      }, error = function(e) FALSE)
    },

    assemble_chunks = function(indicator, urls) {
      n <- length(urls)
      responses <- vector("list", n)
      for (i in seq_len(n)) {
        path <- private$get_chunk_path(indicator, i)
        responses[[i]] <- jsonlite::fromJSON(path, simplifyVector = FALSE)
      }
      list(responses = responses, urls = urls)
    },

    get_data_cache_path = function(indicator) {
      dir <- private$get_cache_dir_path()
      file.path(dir, sprintf("ine_%s_%s_data.rds", indicator, self$lang))
    },

    read_data_cache = function(indicator) {
      path <- private$get_data_cache_path(indicator)
      if (!file.exists(path)) return(NULL)
      tryCatch(readRDS(path), error = function(e) NULL)
    },

    write_data_cache = function(data, dim_filters, indicator) {
      path <- private$get_data_cache_path(indicator)
      dir <- dirname(path)
      if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
      saveRDS(list(data = data, dim_filters = dim_filters), path)
    },

    invalidate_data_cache = function(indicator) {
      path <- private$get_data_cache_path(indicator)
      if (file.exists(path)) unlink(path)
    },

    invalidate_chunk_cache = function(indicator) {
      chunk_dir <- private$get_chunk_dir(indicator)
      manifest_path <- private$get_manifest_path(indicator)
      if (dir.exists(chunk_dir)) unlink(chunk_dir, recursive = TRUE)
      if (file.exists(manifest_path)) unlink(manifest_path)
    },

    invalidate_metadata_cache = function(indicator) {
      path <- private$get_metadata_cache_path(indicator)
      if (file.exists(path)) unlink(path)
    },

    get_metadata_cache_path = function(indicator) {
      dir <- private$get_cache_dir_path()
      file.path(dir, sprintf("ine_%s_%s_meta.json", indicator, self$lang))
    },

    read_metadata_cache = function(indicator) {
      path <- private$get_metadata_cache_path(indicator)
      if (file.exists(path)) {
        tryCatch({
          return(jsonlite::fromJSON(path, simplifyVector = FALSE))
        }, error = function(e) {
          message("Metadata cache read failed: ", e$message)
          return(NULL)
        })
      }
      return(NULL)
    },

    write_metadata_cache = function(metadata_obj, indicator) {
      path <- private$get_metadata_cache_path(indicator)
      dir <- dirname(path)

      if (!dir.exists(dir)) {
        dir.create(dir, recursive = TRUE)
      }

      tryCatch({
        jsonlite::write_json(metadata_obj, path, auto_unbox = TRUE, pretty = TRUE)
        message("Metadata cached at: ", path)
      }, error = function(e) {
        message("Metadata cache write failed: ", e$message)
      })

      invisible(metadata_obj)
    },

    get_catalog_cache_path = function() {
      dir <- private$get_cache_dir_path()
      file.path(dir, paste0("ine_catalog_", self$lang, ".xml"))
    }
  ),

  # ---------- ACTIVE BINDINGS ----------
  active = list(

    #' @field lang Language code (`"PT"` or `"EN"`).
    lang = function(value) {
      if (missing(value)) return(private$.lang)
      private$.lang <- match.arg(value, choices = c("PT", "EN"))
    },

    #' @field use_cache Whether caching is enabled.
    use_cache = function(value) {
      if (missing(value)) return(private$.use_cache)
      if (!is.logical(value) || length(value) != 1) {
        stop("`use_cache` must be TRUE or FALSE")
      }
      private$.use_cache <- value
    },

    #' @field cache_dir Cache directory path, or `NULL` for default.
    cache_dir = function(value) {
      if (missing(value)) return(private$.cache_dir)
      if (!is.null(value) && !is.character(value)) {
        stop("`cache_dir` must be a character string or NULL")
      }
      private$.cache_dir <- value
    },

    #' @field row_limit Default maximum output rows per API request.
    row_limit = function(value) {
      if (missing(value)) return(private$.row_limit)
      if (!is.numeric(value) || length(value) != 1 || value < 1 || value > 1000000L) {
        stop("`row_limit` must be a positive integer no greater than 1 000 000")
      }
      private$.row_limit <- as.integer(value)
    },

    #' @field max_retries Maximum retry attempts for chunk downloads.
    max_retries = function(value) {
      if (missing(value)) return(private$.max_retries)
      if (!is.numeric(value) || length(value) != 1 || value < 1) {
        stop("`max_retries` must be a positive integer")
      }
      private$.max_retries <- as.integer(value)
    },

    #' @field progress_interval Print progress every N chunks during downloads.
    progress_interval = function(value) {
      if (missing(value)) return(private$.progress_interval)
      if (!is.numeric(value) || length(value) != 1 || value < 1) {
        stop("`progress_interval` must be a positive integer")
      }
      private$.progress_interval <- as.integer(value)
    },

    #' @field timeout Timeout in seconds for API requests.
    timeout = function(value) {
      if (missing(value)) return(private$.timeout)
      if (!is.numeric(value) || length(value) != 1 || value < 1) {
        stop("`timeout` must be a positive number")
      }
      private$.timeout <- value
    }
  )
)

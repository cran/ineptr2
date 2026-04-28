# Pure utility functions with no state dependency
# These are internal helpers used by INEClient methods


#' Gracefully handle HTTP request failures
#'
#' Validates connectivity, performs the request, and downgrades
#' HTTP errors to messages instead of stopping.
#'
#' @keywords internal
#' @param request An httr2 request object.
#' @param path Optional file path to save the response body to disk.
#' @return An httr2 response object, or \code{invisible(NULL)} on failure.
gracefully_fail <- function(request, path = NULL) {
  try_GET <- function(request) {
    tryCatch(
      resp <- request |>
        httr2::req_perform(path = path),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }
  is_response <- function(x) {
    inherits(x, "httr2_response")
  }

  # Try for timeout problems
  resp <- try_GET(request)
  if (!is_response(resp)) {
    message(resp)
    return(invisible(NULL))
  }
  # stop if status > 400
  if (httr2::resp_status(resp) >= 400) {
    message(paste(httr2::resp_status(resp), httr2::resp_status_desc(resp), sep = " - "))
    return(invisible(NULL))
  }
  resp
}


#' Process raw INE API responses into tidy dataframe
#'
#' @keywords internal
#' @param raw_data List containing parsed JSON responses and urls from fetch_data_raw()
#' @return Tidy data.frame with INE data
process_ine_data <- function(raw_data) {
  responses <- raw_data$responses
  myurls <- raw_data$urls

  chunk_dfs <- vector("list", length(responses))

  for (i in seq_along(responses)) {
    dados <- tryCatch(
      responses[[i]][[1]][["Dados"]],
      error = function(e) {
        stop(sprintf("Failed to parse response %d (%s): %s", i, myurls[i], e$message),
             call. = FALSE)
      }
    )

    keys <- names(dados)
    lens <- vapply(dados, length, integer(1))
    all_records <- unlist(dados, recursive = FALSE, use.names = FALSE)
    df <- records_to_df(all_records)
    df[["dim_1"]] <- rep(keys, lens)
    chunk_dfs[[i]] <- df
  }

  ret_data <- do.call(rbind, chunk_dfs)

  if (nrow(ret_data) == 0 || ncol(ret_data) == 0) {
    stop("API returned no data. Check that the indicator and dimension filters are correct.",
         call. = FALSE)
  }

  if ("dim_1" %in% names(ret_data)) {
    ret_data <- ret_data[, c("dim_1", setdiff(names(ret_data), "dim_1"))]
  }

  ret_data
}


#' Process raw INE catalog XML into a tibble
#'
#' @keywords internal
#' @param xml_string Character string with the raw catalog XML content.
#' @return A data frame with one row per indicator.
process_ine_catalog <- function(xml_string) {

  doc <- xml2::read_xml(xml_string)
  indicators <- xml2::xml_find_all(doc, ".//indicator")

  if (length(indicators) == 0) {
    rlang::abort("No indicators found in catalog.", class = "ineptr2_error")
  }

  # Vectorized extraction - one call per column across all nodes
  extract_child_text <- function(xpath) {
    xml2::xml_text(xml2::xml_find_first(indicators, xpath))
  }

  catalog <- data.frame(
    indicator_id          = xml2::xml_attr(indicators, "id"),
    varcd                 = extract_child_text("varcd"),
    title                 = extract_child_text("title"),
    description           = extract_child_text("description"),
    theme                 = extract_child_text("theme"),
    subtheme              = extract_child_text("subtheme"),
    keywords              = extract_child_text("keywords"),
    geo_lastlevel         = extract_child_text("geo_lastlevel"),
    last_period_available = extract_child_text("dates/last_period_available"),
    last_update           = extract_child_text("dates/last_update"),
    periodicity           = extract_child_text("periodicity"),
    update_type           = extract_child_text("update_type"),
    bdd_url               = extract_child_text("html/bdd_url"),
    metainfo_url          = extract_child_text("html/metainfo_url"),
    json_dataset          = extract_child_text("json/json_dataset"),
    json_metainfo         = extract_child_text("json/json_metainfo"),
    stringsAsFactors      = FALSE
  )

  message("Catalog loaded: ", nrow(catalog), " indicators")
  catalog
}


#' Normalize dimension filters for consistent comparison
#'
#' @keywords internal
#' @param filters Named list from \code{...} (e.g., \code{list(dim1 = "S7A2022", dim2 = c("11","17"))})
#' @return Named list with lowercase names and sorted character values
normalize_dim_filters <- function(filters) {
  if (length(filters) == 0) return(list())
  names(filters) <- tolower(names(filters))
  lapply(filters, function(x) sort(as.character(x)))
}


#' Check if current dimension filters are a subset of cached filters
#'
#' @keywords internal
#' @param current Named list of current dimension filters (normalized)
#' @param cached Named list of cached dimension filters (normalized)
#' @return TRUE if every value in current is available in cached
is_filter_subset <- function(current, cached) {
  if (length(current) == 0 && length(cached) == 0) return(TRUE)

  for (dim_name in names(current)) {
    if (dim_name %in% names(cached)) {
      if (!all(current[[dim_name]] %in% cached[[dim_name]])) {
        return(FALSE)
      }
    }
  }

  for (dim_name in names(cached)) {
    if (!dim_name %in% names(current)) {
      return(FALSE)
    }
  }

  return(TRUE)
}


#' Filter cached data frame to match current dimension filters
#'
#' @keywords internal
#' @param data Cached data.frame
#' @param current_filters Normalized dimension filters from current request
#' @param dim_values Dimension values tibble from metadata (output of extract_dim_values)
#' @return Filtered data.frame
filter_cached_data <- function(data, current_filters, dim_values) {
  for (dim_name in names(current_filters)) {
    dim_num <- as.integer(gsub("\\D", "", dim_name))
    vals <- current_filters[[dim_name]]

    if (identical(vals, "T")) next

    if (dim_num == 1L) {
      dim1_meta <- dim_values[dim_values$dim_num == "1", ]
      dsg_vals <- dim1_meta$categ_dsg[dim1_meta$categ_cod %in% vals]
      data <- data[data$dim_1 %in% dsg_vals, ]
    } else if (dim_num == 2L) {
      data <- data[data$geocod %in% vals, ]
    } else {
      col <- paste0("dim_", dim_num)
      if (col %in% names(data)) {
        data <- data[data[[col]] %in% vals, ]
      }
    }
  }
  data
}


#' Finalize a chunk by validating and renaming from .part to .json
#'
#' @keywords internal
#' @param temp_path Path to the temporary .part file
#' @param final_path Path to the final .json file
#' @return TRUE on success, FALSE on failure
finalize_chunk <- function(temp_path, final_path) {
  valid <- tryCatch({
    jsonlite::fromJSON(temp_path, simplifyVector = FALSE)
    TRUE
  }, error = function(e) FALSE)

  if (!valid) {
    unlink(temp_path)
    return(FALSE)
  }

  file.rename(temp_path, final_path)
}


#' Extract dimension values from raw metadata
#'
#' @keywords internal
#' @param metadata_raw Raw metadata list from the INE API.
#' @return A data.frame with dimension values.
extract_dim_values <- function(metadata_raw) {
  dims <- metadata_raw[["Dimensoes"]][["Categoria_Dim"]]
  records <- unlist(unlist(dims, recursive = FALSE), recursive = FALSE)
  records_to_df(records)
}


#' Convert a list of named lists to a data.frame
#'
#' Handles NULL values by replacing them with NA, unlike
#' \code{as.data.frame} which silently drops NULL elements.
#'
#' @keywords internal
#' @param records A list of named lists with consistent field names.
#' @return A data.frame.
records_to_df <- function(records) {
  col_names <- unique(unlist(lapply(records, names)))
  cols <- lapply(col_names, function(nm) {
    vapply(records, function(rec) {
      val <- rec[[nm]]
      if (is.null(val)) NA_character_ else as.character(val)
    }, character(1))
  })
  df <- as.data.frame(cols, stringsAsFactors = FALSE)
  names(df) <- col_names
  df
}


#' Calculate dimension lengths from raw metadata
#'
#' @keywords internal
#' @param metadata_raw Raw metadata list from the INE API.
#' @return A data.frame with dim_num and n columns.
calc_dims_length_from_raw <- function(metadata_raw) {
  dim_values <- extract_dim_values(metadata_raw)
  counts <- as.data.frame(table(dim_values$dim_num), stringsAsFactors = FALSE)
  names(counts) <- c("dim_num", "n")
  counts
}

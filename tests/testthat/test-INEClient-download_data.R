test_that("Invalid indicator format is rejected", {
  client <- INEClient$new()
  expect_error(client$download_data("invalid_id"), "7-digit")
  expect_error(client$download_data(123), "7-digit")
})

test_that("download_data downloads to cache and returns metadata", {
  skip_on_cran()
  cache_dir <- tempfile("ineptr2_test_")
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  result <- suppressMessages(
    client$download_data("0011823")
  )

  expect_type(result, "list")
  expect_equal(result$indicator, "0011823")
  expect_true(result$complete)
  expect_true(dir.exists(result$cache_dir))
  expect_true(result$total_chunks >= 1)
})

test_that("download_data enables caching even when use_cache is FALSE", {
  skip_on_cran()
  cache_dir <- tempfile("ineptr2_test_")
  client <- INEClient$new(use_cache = FALSE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  result <- suppressMessages(
    client$download_data("0011823")
  )

  expect_type(result, "list")
  expect_true(result$complete)
  # use_cache should be restored to FALSE after the call

  expect_false(client$use_cache)
})

test_that("download_data with dimension filters", {
  skip_on_cran()
  cache_dir <- tempfile("ineptr2_test_")
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  result <- suppressMessages(
    client$download_data("0008206", dim1 = "S7A1996", dim2 = c("11", "111"))
  )

  expect_type(result, "list")
  expect_true(result$complete)
})

test_that("download_data marks complete when all chunks are already cached", {
  skip_on_cran()
  cache_dir <- tempfile("ineptr2_test_")
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  suppressMessages(client$download_data("0011823"))

  manifest_path <- file.path(cache_dir, "ine_0011823_PT_manifest.json")
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = TRUE)
  manifest$complete <- FALSE
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE)

  expect_message(
    result <- client$download_data("0011823"),
    "All.*chunks already cached"
  )
  expect_true(result$complete)
})

test_that("download_data reports failure when chunk download fails", {
  skip_on_cran()
  cache_dir <- tempfile("ineptr2_test_")
  client <- INEClient$new(use_cache = TRUE, cache_dir = cache_dir, max_retries = 1L)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  call_count <- 0L
  mock_fn <- function(req) {
    call_count <<- call_count + 1L
    if (call_count <= 1L) {
      httr2::response(
        status_code = 200,
        headers = list("content-type" = "application/json"),
        body = charToRaw(jsonlite::toJSON(list(list(
          Sucesso = list(Verdadeiro = "OK"),
          IndicadorCod = "0011823",
          IndicadorNome = "Test",
          Periodic = "Anual",
          PrimeiroPeriodo = "2020",
          UltimoPeriodo = "2023",
          UnidadeMedida = "%",
          Potencia10 = "0",
          PrecisaoDecimal = "2",
          Lingua = "PT",
          DataUltimaAtualizacao = "2023-01-01",
          DataExtracao = "2023-01-01",
          Dimensoes = list(
            Descricao_Dim = list(
              list(dim_num = "1", abrv = "Tempo", versao = "X"),
              list(dim_num = "2", abrv = "Geo", versao = "Y")
            ),
            Categoria_Dim = list(
              list(list(
                list(dim_num = "1", cat_id = "S7A2020", categ_cod = "S7A2020",
                     categ_dsg = "2020", categ_ord = "1", categ_nivel = "1",
                     value_id = "V1")
              )),
              list(list(
                list(dim_num = "2", cat_id = "PT", categ_cod = "PT",
                     categ_dsg = "Portugal", categ_ord = "1", categ_nivel = "1",
                     value_id = "V2")
              ))
            )
          )
        )), auto_unbox = TRUE))
      )
    } else {
      httr2::response(status_code = 500)
    }
  }

  suppressMessages(
    result <- httr2::with_mocked_responses(mock_fn, {
      client$download_data("0011823")
    })
  )
  expect_null(result)
})

test_that("HTTP errors raise ineptr2_api_error in download_data", {
  client <- INEClient$new()
  mock_404 <- function(req) {httr2::response(status_code = 404)}
  expect_error(
    suppressMessages(
      httr2::with_mocked_responses(mock_404, client$download_data("0011823"))
    ),
    class = "ineptr2_api_error"
  )
})

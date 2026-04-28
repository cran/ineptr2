test_that("Invalid indicator format is rejected", {
  client <- INEClient$new()
  expect_error(client$get_metadata("invalid_id"), "7-digit")
  expect_error(client$get_metadata(123), "7-digit")
})

test_that("Indicator exists and call succeded", {
  skip_on_cran()
  client <- INEClient$new()
  expect_type(client$get_metadata("0010003"), "list")
  expect_named(client$get_metadata("0010003"), c('IndicadorCod', 'IndicadorNome', 'Periodic', 'PrimeiroPeriodo', 'UltimoPeriodo', 'UnidadeMedida', 'Potencia10', 'PrecisaoDecimal', 'Lingua', 'DataUltimaAtualizacao', 'DataExtracao'))
})

test_that("HTTP errors raise ineptr2_api_error", {
  client <- INEClient$new()
  mock_404 <- function(req) {httr2::response(status_code = 404)}
  mock_503 <- function(req) {httr2::response(status_code = 503)}
  expect_error(
    suppressMessages(httr2::with_mocked_responses(mock_404, client$get_metadata("0010003"))),
    class = "ineptr2_api_error"
  )
  expect_error(
    suppressMessages(httr2::with_mocked_responses(mock_503, client$get_metadata("0010003"))),
    class = "ineptr2_api_error"
  )
})

mock_catalog_xml <- function() {
  path <- testthat::test_path("fixtures", "mock_catalog.xml")
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

test_that("process_ine_catalog parses mock XML into data frame", {
  catalog <- suppressMessages(ineptr2:::process_ine_catalog(mock_catalog_xml()))
  expect_s3_class(catalog, "data.frame")
  expect_equal(nrow(catalog), 3)
})

test_that("process_ine_catalog extracts correct columns", {
  catalog <- suppressMessages(ineptr2:::process_ine_catalog(mock_catalog_xml()))
  expected_cols <- c("indicator_id", "varcd", "title", "description", "theme",
                     "subtheme", "keywords", "geo_lastlevel",
                     "last_period_available", "last_update", "periodicity",
                     "update_type", "bdd_url", "metainfo_url",
                     "json_dataset", "json_metainfo")
  expect_named(catalog, expected_cols)
})

test_that("process_ine_catalog extracts correct indicator IDs", {
  catalog <- suppressMessages(ineptr2:::process_ine_catalog(mock_catalog_xml()))
  expect_equal(catalog$indicator_id, c("0000164", "0010003", "0008273"))
  expect_equal(catalog$varcd, c("0000164", "0010003", "0008273"))
})

test_that("process_ine_catalog extracts nested date fields", {
  catalog <- suppressMessages(ineptr2:::process_ine_catalog(mock_catalog_xml()))
  expect_equal(catalog$last_update[2], "28-12-2022")
  expect_equal(catalog$last_period_available[2], "7A2023")
})

test_that("process_ine_catalog extracts nested json/html URLs", {
  catalog <- suppressMessages(ineptr2:::process_ine_catalog(mock_catalog_xml()))
  expect_true(grepl("0010003", catalog$json_dataset[2]))
  expect_true(grepl("0010003", catalog$bdd_url[2]))
})

test_that("process_ine_catalog prints count message", {
  expect_message(
    ineptr2:::process_ine_catalog(mock_catalog_xml()),
    "3 indicators"
  )
})

test_that("process_ine_catalog errors for empty catalog", {
  empty_xml <- "<catalog></catalog>"
  expect_error(
    ineptr2:::process_ine_catalog(empty_xml),
    "No indicators found"
  )
})

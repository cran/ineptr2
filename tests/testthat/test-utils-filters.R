# --- is_filter_subset ---

test_that("is_filter_subset returns TRUE for identical filters", {
  f <- list(dim1 = c("A", "B"), dim2 = c("X"))
  expect_true(ineptr2:::is_filter_subset(f, f))
})

test_that("is_filter_subset returns TRUE when both are empty", {
  expect_true(ineptr2:::is_filter_subset(list(), list()))
})

test_that("is_filter_subset returns TRUE for strict subset of values", {
  current <- list(dim1 = "A")
  cached  <- list(dim1 = c("A", "B"))
  expect_true(ineptr2:::is_filter_subset(current, cached))
})

test_that("is_filter_subset returns FALSE when current has values not in cached", {
  current <- list(dim1 = c("A", "C"))
  cached  <- list(dim1 = c("A", "B"))
  expect_false(ineptr2:::is_filter_subset(current, cached))
})

test_that("is_filter_subset returns FALSE when cached has extra dimensions", {
  current <- list(dim1 = "A")
  cached  <- list(dim1 = "A", dim2 = "X")
  expect_false(ineptr2:::is_filter_subset(current, cached))
})

test_that("is_filter_subset returns FALSE when current has extra dimensions", {
  current <- list(dim1 = "A", dim2 = "X")
  cached  <- list(dim1 = "A")
  expect_true(ineptr2:::is_filter_subset(current, cached))
})

test_that("is_filter_subset handles empty current with non-empty cached", {
  expect_false(ineptr2:::is_filter_subset(list(), list(dim1 = "A")))
})

test_that("is_filter_subset handles non-empty current with empty cached", {
  expect_true(ineptr2:::is_filter_subset(list(dim1 = "A"), list()))
})

# --- filter_cached_data ---

test_that("filter_cached_data returns unfiltered data with no filters", {
  data <- data.frame(dim_1 = c("2020", "2021"), geocod = c("PT", "PT"),
                     valor = c(1, 2), stringsAsFactors = FALSE)
  result <- ineptr2:::filter_cached_data(data, list(), data.frame())
  expect_equal(nrow(result), 2)
})

test_that("filter_cached_data skips dim with value 'T'", {
  data <- data.frame(dim_1 = c("2020", "2021"), geocod = c("PT", "PT"),
                     valor = c(1, 2), stringsAsFactors = FALSE)
  result <- ineptr2:::filter_cached_data(data, list(dim1 = "T"), data.frame())
  expect_equal(nrow(result), 2)
})

test_that("filter_cached_data filters dim2 by geocod", {
  data <- data.frame(dim_1 = c("2020", "2020"), geocod = c("PT", "11"),
                     valor = c(1, 2), stringsAsFactors = FALSE)
  result <- ineptr2:::filter_cached_data(data, list(dim2 = "PT"), data.frame())
  expect_equal(nrow(result), 1)
  expect_equal(result$geocod, "PT")
})

test_that("filter_cached_data filters dim1 by looking up categ_dsg", {
  data <- data.frame(dim_1 = c("2020", "2021", "2022"), geocod = "PT",
                     valor = 1:3, stringsAsFactors = FALSE)
  dim_values <- data.frame(
    dim_num = c("1", "1", "1"),
    categ_cod = c("S7A2020", "S7A2021", "S7A2022"),
    categ_dsg = c("2020", "2021", "2022"),
    stringsAsFactors = FALSE
  )
  result <- ineptr2:::filter_cached_data(
    data, list(dim1 = c("S7A2020", "S7A2022")), dim_values
  )
  expect_equal(nrow(result), 2)
  expect_equal(result$dim_1, c("2020", "2022"))
})

test_that("filter_cached_data filters higher dimensions by dim_N column", {
  data <- data.frame(dim_1 = "2020", geocod = "PT",
                     dim_3 = c("A", "B", "C"),
                     valor = 1:3, stringsAsFactors = FALSE)
  result <- ineptr2:::filter_cached_data(
    data, list(dim3 = c("A", "C")), data.frame()
  )
  expect_equal(nrow(result), 2)
  expect_equal(result$dim_3, c("A", "C"))
})

test_that("filter_cached_data ignores missing higher dimension columns", {
  data <- data.frame(dim_1 = "2020", geocod = "PT",
                     valor = 1, stringsAsFactors = FALSE)
  result <- ineptr2:::filter_cached_data(
    data, list(dim5 = "X"), data.frame()
  )
  expect_equal(nrow(result), 1)
})

test_that("filter_cached_data applies multiple filters together", {
  data <- data.frame(
    dim_1 = c("2020", "2020", "2021", "2021"),
    geocod = c("PT", "11", "PT", "11"),
    valor = 1:4, stringsAsFactors = FALSE
  )
  dim_values <- data.frame(
    dim_num = c("1", "1"),
    categ_cod = c("S7A2020", "S7A2021"),
    categ_dsg = c("2020", "2021"),
    stringsAsFactors = FALSE
  )
  result <- ineptr2:::filter_cached_data(
    data, list(dim1 = "S7A2020", dim2 = "PT"), dim_values
  )
  expect_equal(nrow(result), 1)
  expect_equal(result$dim_1, "2020")
  expect_equal(result$geocod, "PT")
})

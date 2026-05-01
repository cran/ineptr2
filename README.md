
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ineptr2 <a href="https://c-matos.github.io/ineptr2/"><img src="man/figures/logo.png" align="right" height="132" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version-last-release/ineptr2?color=blue)](https://CRAN.R-project.org/package=ineptr2)
[![R-CMD-check](https://github.com/c-matos/ineptr2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/c-matos/ineptr2/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/c-matos/ineptr2/branch/main/graph/badge.svg)](https://app.codecov.io/gh/c-matos/ineptr2?branch=main)
<!-- badges: end -->

## Overview

ineptr2 is an R client for the [Statistics Portugal (INE)
API](https://www.ine.pt/xportal/xmain?xpid=INE&xpgid=ine_api&INST=322751522&xlang=en).
INE publishes over 13 000 statistical indicators covering demographics,
health, economy, education, environment and more. The API provides
programmatic access to them.

Working with the API directly can be cumbersome:

- Responses are capped at 1 million returned rows per request. Multiple
  requests may be required for larger indicators.
- Indicator structure (dimensions, codes, time periods) must be
  previously known or be explored through separate metadata endpoints
  before you can build a more specific query.
- There is no built-in way to resume interrupted downloads or avoid
  redundant requests.

ineptr2 handles all of this through a single R6 client:

- **Automatic chunking** - splits large requests and reassembles the
  results transparently. The user only needs to perform one function
  call.
- **File-based caching** - stores raw chunks, processed data, and
  metadata on disk with resume support. If a very large indicator breaks
  mid-download, resume later from the break point.
- **Exploration helpers** - quality of life methods to inspect
  dimensions, preview download size, check for updates, and validate
  indicator codes before fetching any data.

The package targets [version 2 of the INE
API](https://www.ine.pt/xportal/xmain?xpid=INE&xpgid=ine_api_db&INST=719281968&xlang=en),
which extended the functionality of the previous verison with multiple
time values per request, level-based selection, and a much larger
response size (from 40k to 1M rows).

## Installation

``` r
# From CRAN
install.packages("ineptr2")

# Development version from GitHub
# install.packages("devtools")
pak::pak("c-matos/ineptr2")
```

## Quick example

``` r
library(ineptr2)

ine <- INEClient$new()

# Fetch data for a small indicator
df <- ine$get_data("0010003")
head(df)
#>   dim_1 geocod   geodsg ind_string valor
#> 1  2010     PT Portugal      58,31 58.31
#> 2  2011     PT Portugal      60,04 60.04
#> 3  2012     PT Portugal      60,53 60.53
#> 4  2013     PT Portugal      58,92 58.92
#> 5  2014     PT Portugal      62,10  62.1
#> 6  2015     PT Portugal      61,28 61.28
```

For larger indicators, you can filter by dimension to get only what you
need:

``` r
# Resident population, year 2023, NUTS 1 regions only
df <- ine$get_data(
  "0008273",
  dim1 = "S7A2023",
  dim2 = c("PT", "1", "2", "3"),
  dim3 = "T"
)
head(df)
#>   dim_1 geocod                     geodsg dim_3 dim_3_t dim_4      dim_4_t
#> 1  2023      2 Região Autónoma dos Açores     T      HM    81 70 - 74 anos
#> 2  2023      1                 Continente     T      HM    81 70 - 74 anos
#> 3  2023      3 Região Autónoma da Madeira     T      HM    81 70 - 74 anos
#> 4  2023     PT                   Portugal     T      HM    81 70 - 74 anos
#> 5  2023      2 Região Autónoma dos Açores     T      HM    82 75 - 79 anos
#> 6  2023      1                 Continente     T      HM    82 75 - 79 anos
#>   ind_string  valor
#> 1     11 003  11003
#> 2    594 627 594627
#> 3     13 513  13513
#> 4    619 143 619143
#> 5      7 893   7893
#> 6    498 889 498889
```

Use `ine$get_dim_values()` to discover the valid codes for each
dimension.

## Learn more

- [Get started](https://c-matos.github.io/ineptr2/articles/ineptr2.html)
  — full walkthrough: creating a client, exploring indicators,
  downloading data, and caching.
- [How caching
  works](https://c-matos.github.io/ineptr2/articles/caching.html) —
  chunk cache, manifests, resume support, and cache invalidation.
- [What changed from
  ineptR](https://c-matos.github.io/ineptr2/articles/whats_new.html) —
  differences from the previous version.

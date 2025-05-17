
# TMAtools

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of TMAtools is to facilite the efficient and reproducible analysis of TMA data.

## Installation

You can install the development version of TMAtools from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("EDGEResearch-CA/TMAtools")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(TMAtools)
## basic example code
tma_dirs <- c(
 system.file("extdata", "tma1", package = "TMAtools"),
 system.file("extdata", "tma2", package = "TMAtools")
)
# Run the TMAtools pipeline
tmatools(
  tma_dirs = tma_dirs,
  output_dir = "tmatools_output"
)
```


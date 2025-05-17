
# TMAtools

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15447743.svg)](https://doi.org/10.5281/zenodo.15447743)
<!-- badges: end -->

The goal of TMAtools is to facilitate the efficient and reproducible analysis of TMA data.

Documentation and examples on the package website at [edgeresearch-ca.github.io/TMAtools](https://edgeresearch-ca.github.io/TMAtools/).

## Overview

![TMAtools overview fig.](man/figures/TMAtoolsoverview.png)

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


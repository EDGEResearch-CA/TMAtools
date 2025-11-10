# TMAtools

The goal of TMAtools is to facilitate the efficient and reproducible
processing and integration of TMA data at scale.

Documentation and examples are available at
[edgeresearch-ca.github.io/TMAtools](https://edgeresearch-ca.github.io/TMAtools/).

## Overview

![TMAtools overview fig.](reference/figures/TMAtoolsoverview.png)

TMAtools overview fig.

## Installation

You can install the development version of TMAtools from
[GitHub](https://github.com/EDGEResearch-CA/TMAtools) with:

``` r
# install.packages("remotes")
remotes::install_github("EDGEResearch-CA/TMAtools")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(TMAtools)
# 2 directories with TMA files
tma_dirs <- c(
 system.file("extdata", "tma1", package = "TMAtools"),
 system.file("extdata", "tma2", package = "TMAtools")
)
# spreadsheet with translation and consolidation rules
biomarker_rules_file <- system.file(
  "extdata", "biomarker_rules_example.xlsx",
  package = "TMAtools"
)
# Run the TMAtools pipeline
tmatools(
  tma_dirs = tma_dirs,
  biomarker_rules_file = biomarker_rules_file,
  output_dir = "tmatools_output"
)
```

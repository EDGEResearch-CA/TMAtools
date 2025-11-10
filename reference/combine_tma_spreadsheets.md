# Combine TMA spreadsheets

This function combines multiple TMA spreadsheets into a single
spreadsheet that can be used for deconvolution with
[`TMAtools::deconvolute()`](https://edgeresearch-ca.github.io/tmatools/reference/deconvolute.md).

## Usage

``` r
combine_tma_spreadsheets(
  tma_dir,
  output_file = "combined_tma_spreadsheet.xlsx",
  biomarker_sheet_index = 2
)
```

## Arguments

- tma_dir:

  The directory containing the TMA spreadsheets. This directory must
  contain:

  - One or more excel files for the scores of some biomarker (s)

  - One excel file with "metadata" in the file name

  - One excel file with "clean_tma" in the file name

- output_file:

  The name of the output file.

- biomarker_sheet_index:

  The index of the sheet used for ALL files for the biomarker scores.

## Value

List of data frames (invisible).

## Examples

``` r
library(TMAtools)
tma_dir <- system.file("extdata", package = "TMAtools")
combine_tma_spreadsheets(
    tma_dir = tma_dir,
    output_file = "combined_tma_spreadsheet.xlsx",
    biomarker_sheet_index = 2
)
#> Error in combine_tma_spreadsheets(tma_dir = tma_dir, output_file = "combined_tma_spreadsheet.xlsx",     biomarker_sheet_index = 2): FATAL - missing TMA clean map
# check the output Excel file, which should then be used as
# the input for `TMAtools::deconvolute()`
```

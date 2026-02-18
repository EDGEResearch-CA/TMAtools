# Deconvolute a combined TMA spreadsheet

Reads TMA spreadsheet from `tma_file` which must contain one "TMA map"
sheet and one or more biomarker-specific sheets.

## Usage

``` r
deconvolute(tma_file, metadata = NULL, output_file = NULL, tma_id = NULL)
```

## Arguments

- tma_file:

  Path to input Excel spreadsheet. Both .xlsx and .xls formats are
  supported. Must contain one "TMA map" sheet and one or more
  biomarker-specific sheets (eg, output file of
  [`combine_tma_spreadsheets()`](https://edgeresearch-ca.github.io/tmatools/reference/combine_tma_spreadsheets.md)).

- metadata:

  Optional `data.frame` with at least `core_id` and `accession_id`
  columns, plus any other metadata columns that should be added to the
  output (eg, patient age, sex, etc.).

- output_file:

  Optional path to output Excel spreadsheet, which can be used as input
  to
  [`translate_scores()`](https://edgeresearch-ca.github.io/tmatools/reference/translate_scores.md).

- tma_id:

  Optional TMA identifier (column `tma_id` will be added to output).

## Value

A data frame with the deconvoluted data.

## Details

The function will match core IDs from the TMA map sheet with the
biomarker-specific sheets and return a data frame with a "core_id"
column and as many columns for each biomarker as necessary.

For instance, if core ID 1 has 3 values for biomarker A, the output will
contain 3 columns for biomarker A (A.c1, A.c2, A.c3); if another core ID
has 2 values for biomarker A, its corresponding A.c1 and A.c2 columns
will be filled with the values for that core ID and the A.c3 column will
be filled with `NA`.

If there are no values for a given core ID in a biomarker-specific
sheet, the corresponding columns will all be filled with `NA`.

## Examples

``` r
library(TMAtools)
# grab folder with example TMA datasets
tma_dir <- system.file("extdata", "tma1", package = "TMAtools")
# define output files
combined_tma_file <- "combined_tma.xlsx"
deconvoluted_tma_file <- "deconvoluted_tma.xlsx"

# combine TMA datasets
combine_tma_spreadsheets(
 tma_dir = tma_dir,
 output_file = combined_tma_file,
 biomarker_sheet_index = 2,
 valid_biomarkers = c("ER", "p53") # optional, but recommended to avoid misspelling errors
)

# deconvolute combined TMA dataset
deconvoluted_data <- deconvolute(
   tma_file = combined_tma_file,
   output_file = deconvoluted_tma_file
)
print(deconvoluted_data)
#> # A tibble: 17 × 7
#>    core_id ER.c1 ER.c2 ER.c3 p53.c1 p53.c2 p53.c3
#>    <chr>   <chr> <chr> <chr> <chr>  <chr>  <chr> 
#>  1 1       9     9     9     3      0      5     
#>  2 4       2     0     0     8      4      4     
#>  3 10      x     9     0     3      5      5     
#>  4 13      9     1     1     5      1      3     
#>  5 3       2     x     1     5      9      9     
#>  6 12      9     x     9     1      2      4     
#>  7 5       2     2     2     3      2      1     
#>  8 14      1     2     1     3      4      5     
#>  9 2       1     2     2     8      9      x     
#> 10 11      2     x     0     8      9      5     
#> 11 6       x     2     2     5      3      3     
#> 12 9       0     1     x     3      3      2     
#> 13 15      1     1     2     3      x      1     
#> 14 8       0     1     x     x      9      3     
#> 15 17      0     1     2     8      3      2     
#> 16 7       x     1     x     4      4      5     
#> 17 16      x     2     NA    8      4      NA    
```

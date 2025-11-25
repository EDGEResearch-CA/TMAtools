# Deconvolute TMA map

Reads TMA spreadsheet from `tma_file` which must contain one "TMA map"
sheet and one or more biomaker-specific sheets.

## Usage

``` r
deconvolute(
  tma_file,
  metadata = NULL,
  output_file = NULL,
  tma_id = NULL,
  partial_overlap_ok = TRUE
)
```

## Arguments

- tma_file:

  Path to input Excel spreadsheet. Both .xlsx and .xls formats are
  supported. Must contain one "TMA map" sheet and one or more
  biomarker-specific sheets.

- metadata:

  data.frame with core_id and accession_id columns

- output_file:

  Optional path to output Excel spreadsheet.

- tma_id:

  TMA identifier (from the directory's name)

- partial_overlap_ok:

  If FALSE, throws an error if the overlap between TMA map and score
  sheet is only partial (e.g., there is some non-empty value in the
  score sheet that does not match a value in the TMA map).

## Value

A data frame with the deconvoluted data. The function will match core
IDs from the TMA map sheet with the biomarker-specific sheets and return
a data frame with a "core_id" column and as many columns for each
biomarker as necessary.

For instance, if core ID 1 has 3 values for biomarker A, the output will
contain 3 columns for biomarker A (A.c1, A.c2, A.c3); if another core ID
has 2 values for biomaker A, its corresponding A.c1 and A.c2 columns
will be filled with the values for that core ID and the A.c3 column will
be filled with NA.

If there are no values for a given core ID in a biomarker-specific
sheet, the corresponding columns will all be filled with NA.

## Examples

``` r
library(TMAtools)
tma_file <- system.file("extdata", "example.xlsx", package = "TMAtools")
deconvoluted_data <- deconvolute(tma_file)
#> Error: `path` does not exist: ‘’
head(deconvoluted_data)
#> Error: object 'deconvoluted_data' not found
```

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
head(deconvoluted_data)
#> # A tibble: 6 × 13
#>   core_id TP53.c1 TP53.c2 TP53.c3 TP53.c4 TP53.c5 TP53.c6 ER.c1 ER.c2 ER.c3
#>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr>   <chr> <chr> <chr>
#> 1 X1      X1      NA      NA      NA      NA      NA      X1    NA    NA   
#> 2 X2      X2      NA      NA      NA      NA      NA      X2    NA    NA   
#> 3 X3      X3      NA      NA      NA      NA      NA      X3    NA    NA   
#> 4 1       x       4       0       2       NA      NA      x     x     0    
#> 5 4       1       9       2       1       5       8       9     9     x    
#> 6 X4      X4      NA      NA      NA      NA      NA      X4    NA    NA   
#> # ℹ 3 more variables: ER.c4 <chr>, ER.c5 <chr>, ER.c6 <chr>
```

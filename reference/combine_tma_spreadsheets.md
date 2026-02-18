# Combine TMA spreadsheets

This function combines multiple TMA spreadsheets into a single
spreadsheet that can be used for deconvolution with
[`TMAtools::deconvolute()`](https://edgeresearch-ca.github.io/tmatools/reference/deconvolute.md).

## Usage

``` r
combine_tma_spreadsheets(
  tma_dir,
  output_file = "combined_tma_spreadsheet.xlsx",
  biomarker_sheet_index = 2,
  valid_biomarkers = NULL,
  tma_name = NULL
)
```

## Arguments

- tma_dir:

  A path to a single TMA directory, which must contain:

  - `Score sheets`: one or more Excel files with biomarker scores (one
    biomarker per sheet).

  - `Clean map`: an Excel file with "clean_map" in the file name. This
    file corresponds to the sector map of your TMA that only contains
    the core IDs within the corresponding cells. No other annotation
    outside the map is allowed, as `TMAtools` uses the exact positions
    of core IDs to map corresponding scores. Optional:

  - `Metadata`: An Excel file with "metadata" in the name. In a single
    tab, it must contain at least two columns: "core_id" (core IDs that
    appear in the sector map) and "accession_id" (case or patient
    identifiers). Optionally, you can add other columns with additional
    metadata (e.g., age, sex, histotype, block number), which will be
    carried forward to your output files.

- output_file:

  The name of the output file.

- biomarker_sheet_index:

  The index of the sheet used for ALL files for the biomarker scores.

- valid_biomarkers:

  Optional character vector of biomarkers to check against.

- tma_name:

  name of TMA used in error message only.

## Value

List of data frames with combined biomarker data (scores + map). Each
item of the list will be one sheet in the output Excel file.

## Examples

``` r
library(TMAtools)
# grab folder with example TMA datasets
tma_dir <- system.file("extdata", "tma1", package = "TMAtools")
# define output files
combined_tma_file <- "combined_tma.xlsx"

# combine TMA datasets
combined_data_list <- combine_tma_spreadsheets(
 tma_dir = tma_dir,
 output_file = combined_tma_file,
 biomarker_sheet_index = 2,
 valid_biomarkers = c("ER", "p53") # optional, but recommended to avoid misspelling errors
)
print(combined_data_list)
#> $`TMA map`
#> # A tibble: 9 × 13
#>   ``    ``    ``    ``    ``    ``    ``    ``    ``    ``    ``    ``    ``   
#>   <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
#> 1 NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   
#> 2 NA    NA    1     1     1     2     2     NA    6     6     6     7     7    
#> 3 NA    NA    4     3     3     3     2     NA    9     8     8     8     7    
#> 4 NA    NA    4     4     5     5     5     NA    9     9     NA    NA    NA   
#> 5 NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   
#> 6 NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   
#> 7 NA    NA    10    10    10    11    11    NA    15    15    15    16    16   
#> 8 NA    NA    13    12    12    12    11    NA    NA    NA    17    17    17   
#> 9 NA    NA    13    13    14    14    14    NA    NA    NA    NA    NA    NA   
#> 
#> $ER
#> # A tibble: 9 × 13
#>   ``    ``    ``    ``    ``    ``    ``    ``    ``    ``    ``    ``    ``   
#>   <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
#> 1 NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   
#> 2 NA    NA    9     9     9     1     2     NA    x     2     2     x     1    
#> 3 NA    NA    2     2     x     1     2     NA    0     0     1     x     x    
#> 4 NA    NA    0     0     2     2     2     NA    1     x     NA    NA    NA   
#> 5 NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   
#> 6 NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   
#> 7 NA    NA    x     9     0     2     x     NA    1     1     2     x     2    
#> 8 NA    NA    9     9     x     9     0     NA    NA    NA    0     1     2    
#> 9 NA    NA    1     1     1     2     1     NA    NA    NA    NA    NA    NA   
#> 
#> $p53
#> # A tibble: 9 × 13
#>   ``    ``    ``    ``    ``    ``    ``    ``    ``    ``    ``    ``    ``   
#>   <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
#> 1 NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   
#> 2 NA    NA    3     0     5     8     9     NA    5     3     3     4     4    
#> 3 NA    NA    8     5     9     9     x     NA    3     x     9     3     5    
#> 4 NA    NA    4     4     3     2     1     NA    3     2     NA    NA    NA   
#> 5 NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   
#> 6 NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   
#> 7 NA    NA    3     5     5     8     9     NA    3     x     1     8     4    
#> 8 NA    NA    5     1     2     4     5     NA    NA    NA    8     3     2    
#> 9 NA    NA    1     3     3     4     5     NA    NA    NA    NA    NA    NA   
#> 
```

# Translate biomarker scores from numerical to nominal

This function translates numerical scores of biomarkers to nominal
scores.

## Usage

``` r
translate_scores(
  biomarkers_file,
  biomarker_rules_file = NULL,
  output_file = NULL
)
```

## Arguments

- biomarkers_file:

  Path to the Excel file containing biomarker data.

- biomarker_rules_file:

  Path to spreadsheet containing the translation rules for all
  biomarkers. It must contain a sheet named "translation" with columns
  "biomarker", "original_score", "translated_score".

- output_file:

  Optional path to the output file. If NULL, the function will not save
  the output.

## Value

A data frame with translated biomarker scores.

## Examples

``` r
library(TMAtools)
# grab folder with example TMA datasets
tma_dir <- system.file("extdata", "tma1", package = "TMAtools")
# define output files
combined_tma_file <- "combined_tma.xlsx"
deconvoluted_tma_file <- "deconvoluted_tma.xlsx"
consolidated_tma_file <- "consolidated_tma.xlsx"

# combine TMA datasets
combine_tma_spreadsheets(
 tma_dir = tma_dir,
 output_file = combined_tma_file
)

# deconvolute combined TMA dataset
deconvolute(
   tma_file = combined_tma_file,
   output_file = deconvoluted_tma_file
)
#> Error in spreadsheets[[biomarker_name]][seq_len(nrow(tma_map)), seq_len(ncol(tma_map)),     drop = FALSE]: Can't subset columns past the end.
#> ℹ Locations 11 and 12 don't exist.
#> ℹ There are only 10 columns.

# translate numerical biomarker scores to nominal scores
# and consolidate them for each case
consolidated_data <- translate_and_consolidate_scores(
  biomarkers_file = deconvoluted_tma_file,
  required_biomarkers = c("ER", "TP53")
)
#> Error in translate_and_consolidate_scores(biomarkers_file = deconvoluted_tma_file,     required_biomarkers = c("ER", "TP53")): could not find function "translate_and_consolidate_scores"
head(consolidated_data)
#> Error: object 'consolidated_data' not found
```

# Consolidate biomarkers

This function consolidates biomarker scores for each case.

## Usage

``` r
consolidate_scores(
  biomarkers_file = NULL,
  biomarker_rules_file = NULL,
  output_file = NULL,
  biomarkers_data = NULL,
  late_na_ok = FALSE
)
```

## Arguments

- biomarkers_file:

  Path to the Excel file containing biomarker data.

- biomarker_rules_file:

  Path to spreadsheet containing the consolidation rules for all
  biomarkers. It must contain a sheet named "consolidation" with columns
  "biomarker", "rule_type", "rule_value", "consolidated_value".

- output_file:

  Optional path to the output file. If NULL, the function will not save
  the output.

- biomarkers_data:

  Optinally, pass a data.frame or tibble with biomarker data instead of
  passing `biomarkers_file`. Used during re-consolidation in
  [`tmatools()`](https://edgeresearch-ca.github.io/tmatools/reference/tmatools.md).

- late_na_ok:

  If TRUE, NA values do not trigger error. Used during re-consolidation
  in
  [`tmatools()`](https://edgeresearch-ca.github.io/tmatools/reference/tmatools.md).

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
translated_tma_file <- "translated_tma.xlsx"
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

# translate numerical scores to nominal scores
translate_scores(
   biomarkers_file = deconvoluted_tma_file,
   biomarker_rules_file = "tmp/biomarker_rules.xlsx",
   output_file = translated_tma_file
)
#> Error in get_translation_dictionary(biomarker_rules_file = biomarker_rules_file): Biomarker rules file tmp/biomarker_rules.xlsx does not exist.

# and consolidate nominal scores for each case
consolidated_data <- translate_and_consolidate_scores(
  biomarkers_file = deconvoluted_tma_file,
  required_biomarkers = c("ER", "TP53")
)
#> Error in translate_and_consolidate_scores(biomarkers_file = deconvoluted_tma_file,     required_biomarkers = c("ER", "TP53")): could not find function "translate_and_consolidate_scores"
head(consolidated_data)
#> Error: object 'consolidated_data' not found
```

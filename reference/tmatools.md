# TMAtools pipeline

This function runs the TMAtools pipeline for multiple TMA directories.
It combines TMA datasets, deconvolutes the combined dataset, and
translates numerical biomarker scores to nominal scores.

## Usage

``` r
tmatools(
  tma_dirs,
  biomarker_rules_file,
  output_dir = "tmatools_output",
  combined_tma_file = "1_combined_tma.xlsx",
  deconvoluted_tma_file = "2_deconvoluted_tma.xlsx",
  translated_tma_file = "3_translated_tma.xlsx",
  consolidated_tma_file = "4_consolidated_tma.xlsx",
  final_tma_file = "5_final_consolidated_tmas.xlsx",
  biomarker_sheet_index = 2,
  required_biomarkers = c("ER", "TP53")
)
```

## Arguments

- tma_dirs:

  A character vector of TMA directory paths.

- output_dir:

  The directory where the output files will be saved.

- combined_tma_file:

  The name of the combined TMA file.

- deconvoluted_tma_file:

  The name of the deconvoluted TMA file.

- consolidated_tma_file:

  The name of the consolidated TMA file.

- final_tma_file:

  The name of the final file containing the consolidated scores from
  multiple TMAs processed.

- biomarker_sheet_index:

  The index of the biomarker sheet in the TMA file.

- required_biomarkers:

  A character vector of required biomarkers.

## Examples

``` r
library(TMAtools)
tma_dirs <- c(
 system.file("extdata", "tma1", package = "TMAtools"),
 system.file("extdata", "tma2", package = "TMAtools")
)
# Run the TMAtools pipeline
tmatools(
  tma_dirs = tma_dirs,
  output_dir = "tmatools_output"
)
#> Error in tmatools(tma_dirs = tma_dirs, output_dir = "tmatools_output"): argument "biomarker_rules_file" is missing, with no default
```

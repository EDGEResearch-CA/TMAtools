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
  biomarker_sheet_index = 2
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

## Examples

``` r
library(TMAtools)
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
#> 
#> ── Processing TMA: tma1 ────────────────────────────────────────────────────────
#> Adding placeholder column for biomarker PTEN
#> 
#> ── Processing TMA: tma2 ────────────────────────────────────────────────────────
#> # A tibble: 36 × 17
#>    accession_id tma_id core_id age   sex   ER.c1     ER.c2 ER.c3 PTEN.c0 PTEN.c1
#>    <chr>        <chr>  <chr>   <chr> <chr> <chr>     <chr> <chr> <chr>   <chr>  
#>  1 pt1.tma1     tma1   1       1     m     Unk       Unk   Unk   Unk     NA     
#>  2 pt4.tma1     tma1   4       2     f     diffuse … nega… nega… Unk     NA     
#>  3 pt10.tma1    tma1   10      52    f     Unk       Unk   nega… Unk     NA     
#>  4 pt13.tma1    tma1   13      51    f     Unk       foca… foca… Unk     NA     
#>  5 pt3.tma1     tma1   3       2     f     diffuse … Unk   foca… Unk     NA     
#>  6 pt12.tma1    tma1   12      54    f     Unk       Unk   Unk   Unk     NA     
#>  7 pt5.tma1     tma1   5       0     Unk   diffuse … diff… diff… Unk     NA     
#>  8 pt14.tma1    tma1   14      54    f     focal (1… diff… foca… Unk     NA     
#>  9 pt2.tma1     tma1   2       1     m     focal (1… diff… diff… Unk     NA     
#> 10 pt11.tma1    tma1   11      45    f     diffuse … Unk   nega… Unk     NA     
#> # ℹ 26 more rows
#> # ℹ 7 more variables: PTEN.c2 <chr>, er <chr>, p53 <chr>, p53.c1 <chr>,
#> #   p53.c2 <chr>, p53.c3 <chr>, pten <chr>
```

# Consolidate biomarkers from a translated TMA spreadsheet.

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

  Path to the Excel file containing biomarker data (ie, output from
  [`translate_scores()`](https://edgeresearch-ca.github.io/tmatools/reference/translate_scores.md)).

- biomarker_rules_file:

  Path to spreadsheet containing the consolidation rules for all
  biomarkers. It must contain a sheet named "consolidation" with columns
  "biomarker", "rule_type", "rule_value", "consolidated_value". It must
  not be located within any of the TMA directories being processed.

- output_file:

  Optional path to the output file. If NULL, the function will not save
  the output.

- biomarkers_data:

  Optinally, pass a `data.frame` or `tibble` with biomarker data instead
  of passing `biomarkers_file`. Used during re-consolidation in
  [`tmatools()`](https://edgeresearch-ca.github.io/tmatools/reference/tmatools.md)
  (usually not needed by end users).

- late_na_ok:

  If TRUE, NA values do not trigger error. Used during re-consolidation
  in
  [`tmatools()`](https://edgeresearch-ca.github.io/tmatools/reference/tmatools.md)
  (usually not needed by end users). Defaults to FALSE, which triggers
  an error if any NA is present in the scores to be consolidated.

## Value

A data frame with consolidated biomarker scores.

## Details

The consolidation of individual scores will be placed in new columns
with the same name as the biomarker but without the ".c1", ".c2" etc
suffixes. For instance, if the biomarker is "ER", the consolidated score
will be placed in a new column named "er" (lowercase). The original
columns with the ".c1", ".c2" etc suffixes will be retained in the
output.

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
 output_file = combined_tma_file,
 biomarker_sheet_index = 2,
 valid_biomarkers = c("ER", "p53") # optional, but recommended to avoid misspelling errors
)

# deconvolute combined TMA dataset
deconvolute(
   tma_file = combined_tma_file,
   output_file = deconvoluted_tma_file
)

# grab biomarker rules file
biomarker_rules_file <- system.file("extdata", "biomarker_rules_example.xlsx",  package = "TMAtools")

# translate numerical scores to nominal scores
translate_scores(
   biomarkers_file = deconvoluted_tma_file,
   biomarker_rules_file = biomarker_rules_file,
   output_file = translated_tma_file
)
#> Adding placeholder column for biomarker PTEN

# and consolidate nominal scores for each case
consolidated_data <- consolidate_scores(
  biomarkers_file = translated_tma_file,
  output_file = consolidated_tma_file,
  biomarker_rules_file = biomarker_rules_file
)
print(consolidated_data)
#> # A tibble: 17 × 11
#>    core_id ER.c1      ER.c2 ER.c3 er    p53.c1 p53.c2 p53.c3 p53   PTEN.c0 pten 
#>    <chr>   <chr>      <chr> <chr> <chr> <chr>  <chr>  <chr>  <chr> <chr>   <chr>
#>  1 1       Unk        Unk   Unk   Unk   cytop… compl… subcl… muta… Unk     Unk  
#>  2 4       diffuse (… nega… nega… diff… Unk    abnor… abnor… muta… Unk     Unk  
#>  3 10      Unk        Unk   nega… nega… cytop… subcl… subcl… muta… Unk     Unk  
#>  4 13      Unk        foca… foca… foca… subcl… wild … cytop… muta… Unk     Unk  
#>  5 3       diffuse (… Unk   foca… diff… subcl… Unk    Unk    muta… Unk     Unk  
#>  6 12      Unk        Unk   Unk   Unk   wild … overe… abnor… muta… Unk     Unk  
#>  7 5       diffuse (… diff… diff… diff… cytop… overe… wild … muta… Unk     Unk  
#>  8 14      focal (1-… diff… foca… diff… cytop… abnor… subcl… muta… Unk     Unk  
#>  9 2       focal (1-… diff… diff… diff… Unk    Unk    Unk    Unk   Unk     Unk  
#> 10 11      diffuse (… Unk   nega… diff… Unk    Unk    subcl… muta… Unk     Unk  
#> 11 6       Unk        diff… diff… diff… subcl… cytop… cytop… muta… Unk     Unk  
#> 12 9       negative   foca… Unk   foca… cytop… cytop… overe… muta… Unk     Unk  
#> 13 15      focal (1-… foca… diff… diff… cytop… Unk    wild … muta… Unk     Unk  
#> 14 8       negative   foca… Unk   foca… Unk    Unk    cytop… muta… Unk     Unk  
#> 15 17      negative   foca… diff… diff… Unk    cytop… overe… muta… Unk     Unk  
#> 16 7       Unk        foca… Unk   foca… abnor… abnor… subcl… muta… Unk     Unk  
#> 17 16      Unk        diff… Unk   diff… Unk    abnor… Unk    muta… Unk     Unk  
```

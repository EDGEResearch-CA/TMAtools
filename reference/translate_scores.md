# Translate biomarker scores from a deconvoluted TMA spreadsheet.

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

  Path to the Excel file containing biomarker data (ie, output from
  [`deconvolute()`](https://edgeresearch-ca.github.io/tmatools/reference/deconvolute.md)).

- biomarker_rules_file:

  Path to spreadsheet containing the consolidation rules for all
  biomarkers. It must contain a sheet named "consolidation" with columns
  "biomarker", "rule_type", "rule_value", "consolidated_value". It must
  not be located within any of the TMA directories being processed.

- output_file:

  Optional path to the output file, which can be used as input to
  [`consolidate_scores()`](https://edgeresearch-ca.github.io/tmatools/reference/consolidate_scores.md).

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
translated_data <- translate_scores(
   biomarkers_file = deconvoluted_tma_file,
   biomarker_rules_file = biomarker_rules_file,
   output_file = translated_tma_file
)
#> Adding placeholder column for biomarker PTEN

print(translated_data)
#> # A tibble: 17 × 8
#>    core_id ER.c1          ER.c2          ER.c3      p53.c1 p53.c2 p53.c3 PTEN.c0
#>    <chr>   <chr>          <chr>          <chr>      <chr>  <chr>  <chr>  <chr>  
#>  1 1       Unk            Unk            Unk        cytop… compl… subcl… Unk    
#>  2 4       diffuse (>50%) negative       negative   Unk    abnor… abnor… Unk    
#>  3 10      Unk            Unk            negative   cytop… subcl… subcl… Unk    
#>  4 13      Unk            focal (1-50%)  focal (1-… subcl… wild … cytop… Unk    
#>  5 3       diffuse (>50%) Unk            focal (1-… subcl… Unk    Unk    Unk    
#>  6 12      Unk            Unk            Unk        wild … overe… abnor… Unk    
#>  7 5       diffuse (>50%) diffuse (>50%) diffuse (… cytop… overe… wild … Unk    
#>  8 14      focal (1-50%)  diffuse (>50%) focal (1-… cytop… abnor… subcl… Unk    
#>  9 2       focal (1-50%)  diffuse (>50%) diffuse (… Unk    Unk    Unk    Unk    
#> 10 11      diffuse (>50%) Unk            negative   Unk    Unk    subcl… Unk    
#> 11 6       Unk            diffuse (>50%) diffuse (… subcl… cytop… cytop… Unk    
#> 12 9       negative       focal (1-50%)  Unk        cytop… cytop… overe… Unk    
#> 13 15      focal (1-50%)  focal (1-50%)  diffuse (… cytop… Unk    wild … Unk    
#> 14 8       negative       focal (1-50%)  Unk        Unk    Unk    cytop… Unk    
#> 15 17      negative       focal (1-50%)  diffuse (… Unk    cytop… overe… Unk    
#> 16 7       Unk            focal (1-50%)  Unk        abnor… abnor… subcl… Unk    
#> 17 16      Unk            diffuse (>50%) Unk        Unk    abnor… Unk    Unk    
```

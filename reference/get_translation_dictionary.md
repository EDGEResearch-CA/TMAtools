# Get translation dictionary from biomarker rules file

Get translation dictionary from biomarker rules file

## Usage

``` r
get_translation_dictionary(biomarker_rules_file)
```

## Arguments

- biomarker_rules_file:

  Path to spreadsheet containing the translation rules for all
  biomarkers. It must contain a sheet named "translation" with columns
  "biomarker", "original_score", "translated_score".

## Value

data.frame with the translation rules.

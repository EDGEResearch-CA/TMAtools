# Get consolidation data.frame from biomarker rules file

Get consolidation data.frame from biomarker rules file

## Usage

``` r
get_consolidation_rules_df(biomarker_rules_file)
```

## Arguments

- biomarker_rules_file:

  Path to spreadsheet containing the consolidation rules for all
  biomarkers. It must contain a sheet named "consolidation" with columns
  "biomarker", "rule_type", "rule_value", "consolidated_value".

## Value

a data.frame of consolidation rules for each biomarker.

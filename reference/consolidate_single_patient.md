# Assess consolidated score per patient

Assess consolidated score per patient

## Usage

``` r
consolidate_single_patient(rules_df, scores, unknown_values = c("Unk", "x"))
```

## Arguments

- rules_df:

  data.frame with consolidation rules for a single biomarker

- scores:

  character vector with all scores for a given biomarker/patient

- unknow_values:

  character vector with values treated as unknown. Defaults to
  `c("Unk", "x")`.

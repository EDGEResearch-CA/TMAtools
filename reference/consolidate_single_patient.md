# Assess consolidated score per patient

Assess consolidated score per patient

## Usage

``` r
consolidate_single_patient(
  rules_df,
  scores,
  unknown_values = c("Unk", "x"),
  late_na_ok = FALSE
)
```

## Arguments

- rules_df:

  data.frame with consolidation rules for a single biomarker

- scores:

  character vector with all scores for a given biomarker/patient

- late_na_ok:

  If TRUE, NAs do not trigger error. Used for re-consolidation. Defaults
  to `c("Unk", "x")`.

- unknow_values:

  character vector with values treated as unknown.

## Value

consolidated value (as character).

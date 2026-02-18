# Assess MMR status based on IHC biomarker scores

Assess MMR status based on IHC biomarker scores

## Usage

``` r
assess_mmr_status(biomarkers_data)
```

## Arguments

- biomarkers_data:

  data frame with biomarker scores, which must include the following
  columns: "mlh1", "msh2", "msh6", "pms2". The function will add a new
  column "mmr_ihc_4" with the MMR status based on the following rules:

  - If at least one of MSH6, PMS2, MLH1, MSH2 is absent, MMR status is
    'deficient'.

  - If at least one of MSH6, PMS2 is unknown (i.e., "Unk"), MMR status
    is 'Unk'.

  - If all of MSH6, PMS2 are present and none of MSH6, PMS2, MLH1, MSH2
    is absent, MMR status is 'intact'.

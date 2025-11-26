# load package
# library(TMAtools)
devtools::load_all()

# grab folder with example TMA datasets
tma_dir <- here::here("tmp/tma3")
biomarker_rules_file <- here::here("tmp/biomarker_rules_enoc2.xlsx")
output_dir <- here::here("tmp/tma3/test")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
# define output files
combined_tma_file <- stringr::str_glue("{output_dir}/combined_tma.xlsx")
deconvoluted_tma_file <- stringr::str_glue("{output_dir}/deconvoluted_tma.xlsx")
translated_tma_file <- stringr::str_glue("{output_dir}/translated_tma.xlsx")
consolidated_tma_file <- stringr::str_glue("{output_dir}/consolidated_tma.xlsx")

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
  biomarker_rules_file = biomarker_rules_file,
  output_file = translated_tma_file
)

# and consolidate nominal scores for each case
consolidated_data <- consolidate_scores(
  biomarkers_file = translated_tma_file,
  biomarker_rules_file = biomarker_rules_file,
  output_file = consolidated_tma_file
)
head(consolidated_data)

# full thing in one function
tmatools(
  tma_dirs = here::here(c("inst/extdata/tma1", "inst/extdata/tma2")),
  biomarker_rules_file = here::here("inst/extdata/biomarker_rules_enoc.xlsx"),
  output_dir = here::here("tmp/tesigbh")
)

# full thing in one function
tmatools(
  tma_dir = here::here("tmp/example/tma3"),
  biomarker_rules_file = here::here("tmp/example/biomarker_rules_enoc.xlsx"),
  output_dir = here::here("tmp/newtest3")
)

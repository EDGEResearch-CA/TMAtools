set.seed(42)
# Coverage focus:
# - combine_tma_spreadsheets: happy path + file-contract + sheet-index guard
# - deconvolute: contract checks + metadata-optional output
# - translate/consolidate: malformed input and NA policy
# - tmatools: path/metadata validation + single/no-reconsolidation + reconsolidation
# - assess_mmr_status: required columns + deterministic labels
# - utils/template: hidden artifact sanitization and template copy guard

make_no_metadata_tma <- function(source_dir) {
  target <- tempfile("tmp_tma_no_meta_")
  dir.create(target)
  score_files <- list.files(
    path = source_dir,
    full.names = TRUE,
    pattern = "\\.xls[x]?$",
    ignore.case = TRUE
  )
  score_files <- score_files[!grepl("metadata", basename(score_files), ignore.case = TRUE)]
  file.copy(score_files, target)
  meta_hits <- list.files(target, pattern = "metadata", ignore.case = TRUE)
  if (length(meta_hits) > 0) {
    file.remove(file.path(target, meta_hits))
  }
  target
}

copy_tma_for_test <- function(source_dir) {
  target <- tempfile("tma_fixture_")
  dir.create(target)
  files <- list.files(source_dir, full.names = TRUE)
  file.copy(files, target, recursive = TRUE)
  target
}

write_rules_xlsx <- function(sheet_data) {
  tmp <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(sheet_data, path = tmp)
  tmp
}

test_that("combine_tma_spreadsheets runs standalone and writes expected sheets", {
  # Happy path and metadata-optional path in combine stage.
  tma_dir <- system.file("extdata", "tma1", package = "TMAtools")
  out_file <- tempfile(fileext = ".xlsx")

  combined <- combine_tma_spreadsheets(
    tma_dir = tma_dir,
    output_file = out_file,
    biomarker_sheet_index = 2,
    valid_biomarkers = c("ER", "p53")
  )

  expect_true(file.exists(out_file))
  expect_true(all(c("TMA map", "ER", "p53") %in% names(combined)))

  no_meta_dir <- make_no_metadata_tma(tma_dir)
  expect_true(file.exists(no_meta_dir))
  expect_type(
    combine_tma_spreadsheets(
      tma_dir = no_meta_dir,
      output_file = tempfile(fileext = ".xlsx"),
      biomarker_sheet_index = 2,
      valid_biomarkers = c("ER", "p53")
    ),
    "list"
  )
  unlink(no_meta_dir, recursive = TRUE)
})

test_that("combine_tma_spreadsheets enforces file contract edge failures", {
  # Contract guards for map/metadata/score discovery and invalid biomarker sheet names.
  tma_dir <- system.file("extdata", "tma1", package = "TMAtools")
  temp_dir <- tempfile("tma_contract_")
  dir.create(temp_dir)

  good_score <- file.path(tma_dir, "example_er.xlsx")
  good_meta <- file.path(tma_dir, "example_metadata.xlsx")
  good_map <- file.path(tma_dir, "tma1_clean_map.xlsx")
  bad_dir <- tempfile("tma_contract_no_map_")
  dir.create(bad_dir)

  file.copy(good_score, bad_dir)
  file.copy(good_meta, bad_dir)
  expect_error(
    combine_tma_spreadsheets(
      tma_dir = bad_dir,
      output_file = tempfile(fileext = ".xlsx"),
      biomarker_sheet_index = 2
    ),
    "missing TMA clean map"
  )
  unlink(bad_dir, recursive = TRUE)

  two_maps <- tempfile("tma_two_maps_")
  dir.create(two_maps)
  file.copy(c(good_score, good_meta, good_map), two_maps)
  file.copy(good_map, file.path(two_maps, "another_clean_map.xlsx"))
  expect_error(
    combine_tma_spreadsheets(
      tma_dir = two_maps,
      output_file = tempfile(fileext = ".xlsx"),
      biomarker_sheet_index = 2
    ),
    "more than one TMA clean map"
  )
  unlink(two_maps, recursive = TRUE)

  no_scores <- tempfile("tma_no_scores_")
  dir.create(no_scores)
  file.copy(c(good_meta, good_map), no_scores)
  expect_error(
    combine_tma_spreadsheets(
      tma_dir = no_scores,
      output_file = tempfile(fileext = ".xlsx"),
      biomarker_sheet_index = 2
    ),
    "missing score sheets"
  )
  unlink(no_scores, recursive = TRUE)

  two_meta <- tempfile("tma_two_meta_")
  dir.create(two_meta)
  file.copy(good_score, two_meta)
  file.copy(good_map, two_meta)
  file.copy(good_meta, file.path(two_meta, "example_metadata_2.xlsx"))
  file.copy(good_meta, file.path(two_meta, "metadata_backup.xlsx"))
  expect_error(
    combine_tma_spreadsheets(
      tma_dir = two_meta,
      output_file = tempfile(fileext = ".xlsx"),
      biomarker_sheet_index = 2
    ),
    "more than one metadata"
  )
  unlink(two_meta, recursive = TRUE)

  bad_sheet_names <- tempfile("tma_bad_names_")
  dir.create(bad_sheet_names)
  map <- readxl::read_excel(good_map, col_names = FALSE)
  writexl::write_xlsx(list(`TMA map` = map), file.path(bad_sheet_names, "tma1_clean_map.xlsx"))
  writexl::write_xlsx(
    list(`BAD_BIOMARKER` = data.frame(v = c(1, 2, 3))),
    file.path(bad_sheet_names, "example_bad.xlsx")
  )
  writexl::write_xlsx(
    data.frame(core_id = readxl::read_excel(good_meta, col_types = "text")$core_id),
    file.path(bad_sheet_names, "example_metadata.xlsx")
  )
  expect_error(
    combine_tma_spreadsheets(
      tma_dir = bad_sheet_names,
      output_file = tempfile(fileext = ".xlsx"),
      biomarker_sheet_index = 1,
      valid_biomarkers = c("ER", "P53")
    ),
    "do not match any biomarker"
  )
  unlink(bad_sheet_names, recursive = TRUE)
  unlink(temp_dir, recursive = TRUE)
})

test_that("combine_tma_spreadsheets guards biomarker_sheet_index edge cases", {
  # Static and sampled fuzz for invalid sheet index values.
  tma_dir <- system.file("extdata", "tma1", package = "TMAtools")
  bad_indices <- c(0, -1, 1.5, NA_real_, "2")
  for (bad_idx in bad_indices) {
    expect_error(
      combine_tma_spreadsheets(
        tma_dir = tma_dir,
        output_file = tempfile(fileext = ".xlsx"),
        biomarker_sheet_index = bad_idx
      ),
      "biomarker_sheet_index"
    )
  }

  for (bad_idx in sample(c(-5, -2, -0.1, 99, 0, 1.5, NA_real_), size = 20, replace = TRUE)) {
    expect_error(
      combine_tma_spreadsheets(
        tma_dir = tma_dir,
        output_file = tempfile(fileext = ".xlsx"),
        biomarker_sheet_index = bad_idx
      )
    )
  }

  sparse_tma <- tempfile("tma_sparse_")
  dir.create(sparse_tma)
  writexl::write_xlsx(
    list(`TMA map` = data.frame(V1 = c("c1", NA), stringsAsFactors = FALSE)),
    file.path(sparse_tma, "tma1_clean_map.xlsx")
  )
  writexl::write_xlsx(data.frame(v = c(1, 2)), file.path(sparse_tma, "example_er.xlsx"))
  expect_error(
    combine_tma_spreadsheets(
      tma_dir = sparse_tma,
      output_file = tempfile(fileext = ".xlsx"),
      biomarker_sheet_index = 2
    ),
    "doesn't exist"
  )
  unlink(sparse_tma, recursive = TRUE)
})

test_that("end-to-end combine-deconvolute-translate-consolidate succeeds with stable columns", {
  # Pipeline smoke: schema and row stability across all four stages.
  tma_dir <- system.file("extdata", "tma1", package = "TMAtools")
  rules_file <- system.file("extdata", "biomarker_rules_example.xlsx", package = "TMAtools")
  translation_dict <- TMAtools:::get_translation_dictionary(rules_file)
  required_biomarkers <- tolower(names(translation_dict))

  combined_file <- tempfile(fileext = ".xlsx")
  combine_tma_spreadsheets(
    tma_dir = tma_dir,
    output_file = combined_file,
    biomarker_sheet_index = 2,
    valid_biomarkers = names(translation_dict)
  )

  metadata <- readxl::read_excel(file.path(tma_dir, "example_metadata.xlsx"), col_types = "text")
  decon_file <- tempfile(fileext = ".xlsx")
  decon <- deconvolute(
    tma_file = combined_file,
    metadata = metadata,
    output_file = decon_file,
    tma_id = "tma1"
  )

  translated_file <- tempfile(fileext = ".xlsx")
  translated <- translate_scores(
    biomarkers_file = decon_file,
    biomarker_rules_file = rules_file,
    output_file = translated_file
  )
  consolidated_file <- tempfile(fileext = ".xlsx")
  consolidated <- consolidate_scores(
    biomarkers_file = translated_file,
    output_file = consolidated_file,
    biomarker_rules_file = rules_file
  )

  expect_true(file.exists(combined_file))
  expect_true(file.exists(decon_file))
  expect_true(file.exists(translated_file))
  expect_true(file.exists(consolidated_file))
  expect_true(all(c("accession_id", "core_id", "tma_id") %in% names(decon)))
  expect_true(all(required_biomarkers %in% names(consolidated)))
  expect_equal(nrow(decon), nrow(consolidated))
  expect_false(anyNA(consolidated$er))
})

test_that("deconvolute enforces TMA map contract and handles metadata omission", {
  # deconvolute map contract + no-metadata output shape.
  combined <- tempfile(fileext = ".xlsx")
  tma_dir <- system.file("extdata", "tma1", package = "TMAtools")
  combine_tma_spreadsheets(
    tma_dir = tma_dir,
    output_file = combined,
    biomarker_sheet_index = 2,
    valid_biomarkers = c("ER", "p53", "PTEN")
  )

  decon_without_meta <- tempfile(fileext = ".xlsx")
  decon_data <- deconvolute(
    tma_file = combined,
    metadata = NULL,
    output_file = decon_without_meta
  )
  expect_false("accession_id" %in% names(decon_data))
  expect_true(any(startsWith(names(decon_data), "ER.c")))

  non_tma_map <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(score_only = data.frame(v = 1)), non_tma_map)
  expect_error(
    deconvolute(
      tma_file = non_tma_map,
      output_file = tempfile(fileext = ".xlsx")
    ),
    "must contain a 'TMA map' sheet"
  )
})

test_that("translate/consolidate regressions are guarded on bad inputs", {
  # Hard-fail on unknown translation values, and NA policy in consolidation.
  rules_file <- system.file("extdata", "biomarker_rules_example.xlsx", package = "TMAtools")
  bad_raw <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(data.frame(core_id = "c1", ER.c1 = "BAD_SCORE"), bad_raw)
  expect_error(
    translate_scores(
      biomarkers_file = bad_raw,
      biomarker_rules_file = rules_file
    ),
    "not covered"
  )

  tma_dir <- system.file("extdata", "tma1", package = "TMAtools")
  combined_file <- tempfile(fileext = ".xlsx")
  combine_tma_spreadsheets(
    tma_dir = tma_dir,
    output_file = combined_file,
    biomarker_sheet_index = 2,
    valid_biomarkers = names(TMAtools:::get_translation_dictionary(rules_file))
  )
  metadata <- readxl::read_excel(file.path(tma_dir, "example_metadata.xlsx"), col_types = "text")
  decon_file <- tempfile(fileext = ".xlsx")
  deconvolute(
    tma_file = combined_file,
    metadata = metadata,
    output_file = decon_file,
    tma_id = "tma1"
  )
  translated_file <- tempfile(fileext = ".xlsx")
  translate_scores(
    biomarkers_file = decon_file,
    biomarker_rules_file = rules_file,
    output_file = translated_file
  )

  translated <- readxl::read_excel(translated_file, col_types = "text")
  translated$ER.c1[1] <- NA_character_
  with_na <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(translated, with_na)

  expect_error(
    consolidate_scores(
      biomarkers_file = with_na,
      biomarker_rules_file = rules_file
    ),
    "NA"
  )
  expect_silent(
    consolidate_scores(
      biomarkers_file = with_na,
      biomarker_rules_file = rules_file,
      late_na_ok = TRUE
    )
  )

  # Randomized token fuzz: every iteration must fail translation with unknown score tokens.
  er_cols <- grep("^ER\\.c\\d+$", names(translated), ignore.case = TRUE, value = TRUE)
  expect_true(length(er_cols) >= 1)
  for (iter in seq_len(12)) {
    fuzz <- translated
    row_idx <- sample(seq_len(nrow(fuzz)), 1)
    col_idx <- sample(er_cols, 1)
    fuzz[[col_idx]][row_idx] <- paste0("BAD_", iter, "_", sample(c("x", "NA", "Q"), 1))
    fuzz_file <- tempfile(fileext = ".xlsx")
    writexl::write_xlsx(fuzz, fuzz_file)
    expect_error(
      translate_scores(
        biomarkers_file = fuzz_file,
        biomarker_rules_file = rules_file
      ),
      "not covered"
    )
  }
})

test_that("translate_scores supports quantitative translation and rejects non-quantitative noise", {
  # Quantitative dictionary allows numeric scores through and rejects token noise.
  rules_file <- write_rules_xlsx(
    list(
      translation = data.frame(
        biomarker = "Ki67",
        original_score = c("quantitative", "Unk"),
        translated_score = c("quantitative", "Unk"),
        stringsAsFactors = FALSE
      )
    )
  )
  quant_file_bad <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    data.frame(core_id = c("core_1", "core_2"), Ki67.c1 = c("3", "x")),
    quant_file_bad
  )
  expect_error(
    translate_scores(
      biomarkers_file = quant_file_bad,
      biomarker_rules_file = rules_file
    ),
    "not covered"
  )

  quant_file_ok <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    data.frame(core_id = c("core_1", "core_2"), Ki67.c1 = c("3", "4.5")),
    quant_file_ok
  )
  translated <- translate_scores(
    biomarkers_file = quant_file_ok,
    biomarker_rules_file = rules_file
  )
  expect_identical(translated$Ki67.c1, c("3", "4.5"))
})

test_that("consolidate_scores validates quantitative mean behavior and no-column fallback", {
  # Mean rule path and explicit fallback when biomarker columns are absent.
  mean_rule_file <- write_rules_xlsx(
    list(
      consolidation = data.frame(
        biomarker = "Ki67",
        rule_type = "mean",
        rule_value = NA_character_,
        consolidated_value = "mean",
        stringsAsFactors = FALSE
      )
    )
  )

  quant_data <- data.frame(
    core_id = "core_1",
    Ki67.c1 = "1",
    Ki67.c2 = "3",
    Ki67.c3 = "Unk",
    stringsAsFactors = FALSE
  )
  quant_file <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(quant_data, quant_file)
  meaned <- consolidate_scores(
    biomarkers_file = quant_file,
    biomarker_rules_file = mean_rule_file
  )
  expect_identical(meaned$ki67, "2")

  no_cols <- data.frame(core_id = "core_1", stringsAsFactors = FALSE)
  no_cols_file <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(no_cols, no_cols_file)
  missing_cols <- consolidate_scores(
    biomarkers_file = no_cols_file,
    biomarker_rules_file = mean_rule_file
  )
  expect_identical(missing_cols$ki67, "Unk")
})

test_that("consolidate_scores rejects mixed mean/qualitative rule blocks", {
  # Guard rails against ambiguous mixed rule semantics.
  bad_rules <- write_rules_xlsx(
    list(
      consolidation = data.frame(
        biomarker = c("Ki67", "Ki67"),
        rule_type = c("mean", "any"),
        rule_value = c(NA_character_, "3"),
        consolidated_value = c("mean", "high"),
        stringsAsFactors = FALSE
      )
    )
  )
  expect_error(
    {
      bad_data <- tempfile(fileext = ".xlsx")
      writexl::write_xlsx(data.frame(core_id = "core_1", Ki67.c1 = "3"), bad_data)
      consolidate_scores(
        biomarkers_file = bad_data,
        biomarker_rules_file = bad_rules
      )
    },
    "cannot mix qualitative scores with mean calculation"
  )
})

test_that("deconvolute handles regex-special biomarker names as literals", {
  # Verify literal sheet names like '+' are not interpreted as regex in ordering.
  tma_file <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    list(
      `TMA map` = data.frame(V1 = c("core_a", "core_a"), stringsAsFactors = FALSE),
      `ER+` = data.frame(V1 = c("8", "9"), stringsAsFactors = FALSE)
    ),
    tma_file,
    col_names = FALSE
  )
  decon <- deconvolute(
    tma_file = tma_file,
    metadata = NULL
  )
  expect_true(all(c("core_id", "ER+.c1", "ER+.c2") %in% names(decon)))
  expect_identical(decon$`ER+.c1`, "8")
  expect_identical(decon$`ER+.c2`, "9")
  unlink(tma_file)
})

test_that("consolidate_scores handles very wide replicate matrices", {
  # Wide replicate vectors should consolidate deterministically without truncation.
  n_replicates <- 250
  high_replicate_rules <- write_rules_xlsx(
    list(
      consolidation = data.frame(
        biomarker = "Ki67",
        rule_type = "mean",
        rule_value = NA_character_,
        consolidated_value = "mean",
        stringsAsFactors = FALSE
      )
  )
  )

  row_one <- rep("x", n_replicates)
  row_one[1:2] <- c("1", "3")
  row_two <- rep("Unk", n_replicates)
  row_three <- rep("4", n_replicates)

  high_replicate_data <- as.data.frame(
    rbind(row_one, row_two, row_three),
    stringsAsFactors = FALSE
  )
  names(high_replicate_data) <- paste0("Ki67.c", seq_len(n_replicates))
  high_replicate_data$core_id <- paste0("core_", seq_len(3))
  high_replicate_data <- high_replicate_data[, c(
    "core_id",
    paste0("Ki67.c", seq_len(n_replicates))
  ), drop = FALSE]

  high_replicate_file <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(high_replicate_data, high_replicate_file)

  high_recon <- consolidate_scores(
    biomarkers_file = high_replicate_file,
    biomarker_rules_file = high_replicate_rules
  )

  expect_identical(high_recon$ki67, c("2", "Unk", "4"))
})

test_that("tmatools orchestration succeeds across multiple TMAs with stable final columns", {
  # Multi-TMA orchestration and final column ordering contract.
  tma_dirs <- c(
    system.file("extdata", "tma1", package = "TMAtools"),
    system.file("extdata", "tma1b", package = "TMAtools")
  )
  rules_file <- system.file("extdata", "biomarker_rules_example.xlsx", package = "TMAtools")
  output_dir <- tempfile("tmatools_multi_")
  result <- tmatools(
    tma_dirs = tma_dirs,
    biomarker_rules_file = rules_file,
    output_dir = output_dir
  )

  expect_true(file.exists(file.path(output_dir, "5_final_consolidated_tmas.xlsx")))
  expect_identical(colnames(result)[1:3], c("accession_id", "tma_id", "core_id"))
  expect_true(all(c("er", "p53", "pten") %in% colnames(result)))
  expect_true("age" %in% colnames(result))
})

test_that("tmatools validates metadata file presence and required columns", {
  # Missing/invalid metadata must fail before downstream spreadsheet processing.
  rules_file <- system.file("extdata", "biomarker_rules_example.xlsx", package = "TMAtools")
  broken_dir <- copy_tma_for_test(system.file("extdata", "tma1", package = "TMAtools"))
  bad_meta <- file.path(broken_dir, "example_metadata.xlsx")
  writexl::write_xlsx(data.frame(accession_id = "X1", stringsAsFactors = FALSE), bad_meta)
  expect_error(
    tmatools(
      tma_dirs = broken_dir,
      biomarker_rules_file = rules_file,
      output_dir = tempfile("tmatools_bad_meta_")
    ),
    "required columns"
  )
  unlink(broken_dir, recursive = TRUE)

  no_meta_dir <- copy_tma_for_test(system.file("extdata", "tma1", package = "TMAtools"))
  file.remove(file.path(no_meta_dir, "example_metadata.xlsx"))
  expect_error(
    tmatools(
      tma_dirs = no_meta_dir,
      biomarker_rules_file = rules_file,
      output_dir = tempfile("tmatools_no_meta_")
    ),
    "No metadata file found"
  )
  unlink(no_meta_dir, recursive = TRUE)

  duplicate_meta_dir <- copy_tma_for_test(system.file("extdata", "tma1", package = "TMAtools"))
  file.copy(
    file.path(duplicate_meta_dir, "example_metadata.xlsx"),
    file.path(duplicate_meta_dir, "example_metadata_backup.xlsx")
  )
  expect_error(
    tmatools(
      tma_dirs = duplicate_meta_dir,
      biomarker_rules_file = rules_file,
      output_dir = tempfile("tmatools_multi_meta_")
    ),
    "Multiple metadata files"
  )
  unlink(duplicate_meta_dir, recursive = TRUE)
})

test_that("get_biomarker_rules_template writes template and enforces overwrite policy", {
  # Explicitly tests success path and explicit overwrite guard.
  target <- tempfile(fileext = ".xlsx")
  expect_true(TMAtools:::get_biomarker_rules_template(target, overwrite = FALSE))
  expect_error(
    TMAtools:::get_biomarker_rules_template(target, overwrite = FALSE),
    "Could not copy template biomarker rules file"
  )
  expect_true(TMAtools:::get_biomarker_rules_template(target, overwrite = TRUE))
  expect_true(file.exists(target))
})

test_that("tmatools rejects invalid input paths and exercises single/no-reconsolidation branches", {
  # Path checks, single-TMA path, and branch where no accession is replicated.
  rules_file <- system.file("extdata", "biomarker_rules_example.xlsx", package = "TMAtools")

  expect_error(
    tmatools(
      tma_dirs = "/path/does/not/exist",
      biomarker_rules_file = rules_file,
      output_dir = tempfile("tmatools_invalid_")
    ),
    "Directory does not exist"
  )

  single_tma <- system.file("extdata", "tma1", package = "TMAtools")
  output_single <- tempfile("tmatools_single_")
  single <- tmatools(
    tma_dirs = single_tma,
    biomarker_rules_file = rules_file,
    output_dir = output_single
  )
  expect_true(file.exists(file.path(output_single, "5_final_consolidated_tmas.xlsx")))
  expect_true(all(c("accession_id", "tma_id") %in% colnames(single)))
  expect_true("pten" %in% colnames(single))

  no_replicates <- tmatools(
    tma_dirs = c(
      system.file("extdata", "tma1", package = "TMAtools"),
      system.file("extdata", "tma2", package = "TMAtools")
    ),
    biomarker_rules_file = rules_file,
    output_dir = output_norepl <- tempfile("tmatools_norepl_")
  )
  expect_true(file.exists(file.path(output_norepl, "5_final_consolidated_tmas.xlsx")))
  expect_true(all(c("accession_id", "tma_id") %in% colnames(no_replicates)))
  expect_false(any(duplicated(no_replicates$accession_id)))
  expect_true(nrow(no_replicates) >= 30)
})

test_that("tmatools rejects non-integer biomarker_sheet_index values", {
  # Contract test for orchestration index forwarding into combine stage.
  rules_file <- system.file("extdata", "biomarker_rules_example.xlsx", package = "TMAtools")
  bad_indices <- c(0, -1, 1.5, NA_real_, "2")

  for (bad_idx in bad_indices) {
    expect_error(
      tmatools(
        tma_dirs = system.file("extdata", "tma1", package = "TMAtools"),
        biomarker_rules_file = rules_file,
        output_dir = tempfile("tmatools_bad_index_"),
        biomarker_sheet_index = bad_idx
      ),
      "biomarker_sheet_index"
    )
  }
})

test_that("tmatools rejects inconsistent translation/consolidation biomarkers in rule files", {
  # Startup guard for translation/consolidation biomarker set mismatch.
  rules_file <- system.file("extdata", "biomarker_rules_example.xlsx", package = "TMAtools")
  rules_bad <- tempfile(fileext = ".xlsx")
  translation <- readxl::read_excel(rules_file, sheet = "translation", col_types = "text")
  consolidation <- readxl::read_excel(rules_file, sheet = "consolidation", col_types = "text")
  consolidation <- consolidation[consolidation$biomarker != "ER", ]

  writexl::write_xlsx(
    list(
      translation = translation,
      consolidation = consolidation
    ),
    rules_bad
  )

  expect_error(
    tmatools(
      tma_dirs = system.file("extdata", "tma1", package = "TMAtools"),
      biomarker_rules_file = rules_bad,
      output_dir = tempfile("tmatools_bad_rules_")
    ),
    "Biomarkers in (consolidation but not in translation rules|translation but not in consolidation rules)"
  )
})

test_that("tmatools reconsolidates overlapping accession IDs across TMAs", {
  # Reconsolidation branch: duplicated accession IDs should still emit final merge output and grouped states.
  rules_file <- system.file("extdata", "biomarker_rules_example.xlsx", package = "TMAtools")

  tma_one <- copy_tma_for_test(system.file("extdata", "tma1", package = "TMAtools"))
  tma_two <- copy_tma_for_test(system.file("extdata", "tma2", package = "TMAtools"))

  meta_one <- readxl::read_excel(file.path(tma_one, "example_metadata.xlsx"), col_types = "text")
  meta_two <- readxl::read_excel(file.path(tma_two, "example_metadata.xlsx"), col_types = "text")
  meta_two$accession_id <- meta_one$accession_id[1]
  writexl::write_xlsx(meta_two, file.path(tma_two, "example_metadata.xlsx"))

  output_dir <- tempfile("tmatools_recon_")
  recon <- tmatools(
    tma_dirs = c(tma_one, tma_two),
    biomarker_rules_file = rules_file,
    output_dir = output_dir
  )

  expect_true(file.exists(file.path(output_dir, "5_final_consolidated_tmas.xlsx")))
  expect_true(any(duplicated(recon$accession_id)))
  expect_true(all(c("accession_id", "tma_id", "core_id") %in% colnames(recon)))
  expect_true(all(!is.na(recon$accession_id)))

  unlink(tma_one, recursive = TRUE)
  unlink(tma_two, recursive = TRUE)
})

test_that("utility behavior and tmatools metadata contract regressions are safe", {
  # ENOC artifact sanitization + missing required metadata columns path.
  rules_file <- system.file("extdata", "biomarker_rules_example.xlsx", package = "TMAtools")
  source_tma <- system.file("extdata", "tma1", package = "TMAtools")

  enoc_rules <- system.file("extdata", "biomarker_rules_enoc.xlsx", package = "TMAtools")
  consolidation_rules <- TMAtools:::get_consolidation_rules_df(enoc_rules)
  artifact_free <- unlist(consolidation_rules[, c("biomarker", "rule_type", "rule_value", "consolidated_value")])
  expect_false(any(grepl("_x000d_", artifact_free, fixed = TRUE)))

  bad_tma <- copy_tma_for_test(source_tma)
  bad_metadata <- file.path(bad_tma, "example_metadata.xlsx")
  meta <- data.frame(core_id = readxl::read_excel(bad_metadata, col_types = "text")$core_id)
  names(meta) <- "core_id"
  writexl::write_xlsx(meta, bad_metadata)

  expect_error(
    tmatools(
      tma_dirs = bad_tma,
      biomarker_rules_file = rules_file,
      output_dir = tempfile("tmatools_out_")
    ),
    "required columns"
  )
  unlink(bad_tma, recursive = TRUE)
})

test_that("rule readers fail with malformed rule files before downstream processing", {
  # Rule loading stops before translation/consolidation to avoid deep pipeline errors.
  bad_translation_rules <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    list(
      translation = data.frame(
        biomarker = "ER",
        original_score = "1",
        extra = "A",
        stringsAsFactors = FALSE
      )
    ),
    bad_translation_rules
  )
  bad_biomarkers <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    data.frame(core_id = "1", ER.c1 = "1"),
    bad_biomarkers
  )
  expect_error(
    translate_scores(
      biomarkers_file = bad_biomarkers,
      biomarker_rules_file = bad_translation_rules
    ),
    "missing columns required for translation"
  )

  bad_consolidation_rules <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    list(
      translation = data.frame(
        biomarker = "ER",
        original_score = "1",
        translated_score = "pos"
      ),
      consolidation = data.frame(
        biomarker = "ER",
        rule_type = "any",
        rule_value = "neg",
        consolidated_value = "neg",
        stringsAsFactors = FALSE
      )
    ),
    bad_consolidation_rules
  )
  expect_error(
    TMAtools:::get_consolidation_rules_df(bad_consolidation_rules),
    "does not have an 'else' rule"
  )
})

test_that("rule text sanitization removes hidden control artifacts", {
  # Utility-level fuzz check for hidden control chars in rule text.
  dirty_rules <- data.frame(
    biomarker = c("ER_x000d_", "p53"),
    rule_type = c("else", "all"),
    rule_value = c("neg\r", "pos_x000d_"),
    consolidated_value = c("0", "1"),
    stringsAsFactors = FALSE
  )
  sanitized <- TMAtools:::.sanitize_rules_df(
    dirty_rules,
    file_path = "tmp_rules.xlsx",
    columns = c("biomarker", "rule_type", "rule_value", "consolidated_value")
  )

  expect_false(any(grepl("_x000d_", as.character(unlist(sanitized)))))
  expect_false(any(grepl("[[:cntrl:]]", as.character(unlist(sanitized)))))
  expect_equal(sanitized$biomarker[1], "ER")
  expect_equal(sanitized$rule_value[1], "neg")
})

test_that("assess_mmr_status validates required markers and classifies deterministic states", {
  # Domain rule check: required columns + deterministic status mapping.
  expect_error(
    assess_mmr_status(
      data.frame(
        mlh1 = "present",
        pms2 = "present",
        msh6 = "present"
      )
    ),
    "missing"
  )

  assessed <- assess_mmr_status(
    data.frame(
      mlh1 = c("present", "present", "present", "absent"),
      msh2 = c("present", "present", "present", "present"),
      msh6 = c("present", "Unk", "present", "present"),
      pms2 = c("present", "present", "absent", "present"),
      stringsAsFactors = FALSE
    )
  )
  expect_identical(
    assessed$mmr_ihc_4,
    c("intact", "Unk", "deficient", "deficient")
  )
})

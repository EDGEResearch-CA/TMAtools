#' @title TMAtools pipeline
#' @description
#' This function runs the TMAtools pipeline for multiple TMA directories.
#' It combines TMA datasets, deconvolutes the combined dataset,
#' and translates numerical biomarker scores to nominal scores.
#' @param tma_dirs A character vector of TMA directory paths.
#' @param output_dir The directory where the output files will be saved.
#' @param combined_tma_file The name of the combined TMA file.
#' @param deconvoluted_tma_file The name of the deconvoluted TMA file.
#' @param consolidated_tma_file The name of the consolidated TMA file.
#' @param final_tma_file The name of the final file containing the consolidated scores from multiple TMAs processed.
#' @param biomarker_sheet_index The index of the biomarker sheet in the TMA file.
#' @return NULL
#' @export
#' @examples
#' library(TMAtools)
#' tma_dirs <- c(
#'  system.file("extdata", "tma1", package = "TMAtools"),
#'  system.file("extdata", "tma2", package = "TMAtools")
#' )
#' # spreadsheet with translation and consolidation rules
#' biomarker_rules_file <- system.file(
#'   "extdata", "biomarker_rules_example.xlsx",
#'   package = "TMAtools"
#' )
#' # Run the TMAtools pipeline
#' tmatools(
#'   tma_dirs = tma_dirs,
#'   biomarker_rules_file = biomarker_rules_file,
#'   output_dir = "tmatools_output"
#' )
tmatools <- function(
  tma_dirs,
  biomarker_rules_file,
  output_dir = "tmatools_output",
  combined_tma_file = "1_combined_tma.xlsx",
  deconvoluted_tma_file = "2_deconvoluted_tma.xlsx",
  translated_tma_file = "3_translated_tma.xlsx",
  consolidated_tma_file = "4_consolidated_tma.xlsx",
  final_tma_file = "5_final_consolidated_tmas.xlsx",
  biomarker_sheet_index = 2
) {
  for (tma_dir in tma_dirs) {
    if (!dir.exists(tma_dir)) {
      cli::cli_abort("Directory does not exist: ", tma_dir)
    }
  }
  stopifnot(file.exists(biomarker_rules_file))
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  combined_prefix <- tools::file_path_sans_ext(
    basename(combined_tma_file)
  )
  deconvoluted_prefix <- tools::file_path_sans_ext(
    basename(deconvoluted_tma_file)
  )
  translated_prefix <- tools::file_path_sans_ext(
    basename(translated_tma_file)
  )
  consolidated_prefix <- tools::file_path_sans_ext(
    basename(consolidated_tma_file)
  )
  # grab biomarkers in translation dict and consolidation rules
  translation_dict_biomarkers <- names(
    get_translation_dictionary(biomarker_rules_file)
  )
  consolidation_rules_biomarkers <- unique(
    get_consolidation_rules_df(biomarker_rules_file)$biomarker
  )
  # ensure all biomarkers in both translation and consolidation vectors
  in_trans_but_not_in_cons <- setdiff(
    translation_dict_biomarkers,
    consolidation_rules_biomarkers
  )
  in_cons_but_not_in_trans <- setdiff(
    consolidation_rules_biomarkers,
    translation_dict_biomarkers
  )
  if (length(in_trans_but_not_in_cons) > 0) {
    msg <- paste0(
      "Biomarkers in translation but not in consolidation rules: ",
      paste0(in_trans_but_not_in_cons, collapse = ", ")
    )
    cli::cli_abort(msg)
  }
  if (length(in_cons_but_not_in_trans) > 0) {
    msg <- paste0(
      "Biomarkers in consolidaton but not in translation rules: ",
      paste0(in_cons_but_not_in_trans, collapse = ", ")
    )
    cli::cli_abort(msg)
  }

  # Keep track of metadata columns
  metadata_columns <- character()

  # run TMAtools pipeline for each TMA directory
  all_spreadsheets <- setNames(
    vector("list", length(tma_dirs)),
    basename(tma_dirs)
  )
  for (tma_dir in tma_dirs) {
    cli::cli_h1(
      paste0(
        "Processing TMA: ",
        basename(tma_dir)
      )
    )
    metadata_file <- list.files(
      path = tma_dir,
      pattern = "*metadata*",
      full.names = TRUE
    )
    if (length(metadata_file) == 0) {
      cli::cli_abort("No metadata file found in: ", tma_dir)
    }
    if (length(metadata_file) > 1) {
      cli::cli_abort("Multiple metadata files found in: ", tma_dir)
    }
    metadata <- readxl::read_excel(metadata_file[1], sheet = 1)
    if (!all(c("core_id", "accession_id") %in% colnames(metadata))) {
      cli::cli_abort(
        "Metadata file missing some of the required columns (core_id, accession_id): ",
        metadata_file[1]
      )
    }
    # keep track of new metadata columns
    metadata_columns <- c(
      metadata_columns,
      setdiff(
        colnames(metadata),
        c(metadata_columns, "tma_id", "core_id", "accession_id")
      )
    )
    metadata$core_id <- as.character(metadata$core_id)
    metadata$accession_id <- as.character(metadata$accession_id)

    # combine TMA datasets
    .combined_tma_file <- file.path(
      dirname(combined_tma_file),
      paste0(
        combined_prefix,
        "_",
        basename(tma_dir),
        ".xlsx"
      )
    )
    .deconvoluted_tma_file <- file.path(
      dirname(deconvoluted_tma_file),
      paste0(
        deconvoluted_prefix,
        "_",
        basename(tma_dir),
        ".xlsx"
      )
    )
    .translated_tma_file <- file.path(
      dirname(translated_tma_file),
      paste0(
        translated_prefix,
        "_",
        basename(tma_dir),
        ".xlsx"
      )
    )
    .consolidated_tma_file <- file.path(
      dirname(consolidated_tma_file),
      paste0(
        consolidated_prefix,
        "_",
        basename(tma_dir),
        ".xlsx"
      )
    )
    combine_tma_spreadsheets(
      tma_dir = tma_dir,
      output_file = file.path(
        output_dir,
        .combined_tma_file
      ),
      biomarker_sheet_index = biomarker_sheet_index,
      valid_biomarkers = translation_dict_biomarkers
    )
    # check that all biomarkers have translation/consolidation rules
    # (just need to check translation since translation and consolidation
    # have the same biomarkers)
    biomarkers_with_data <- readxl::excel_sheets(
      file.path(
        output_dir,
        .combined_tma_file
      )
    )
    biomarkers_with_data <- biomarkers_with_data[
      biomarkers_with_data != "TMA map"
    ]
    in_data_but_not_in_trans <- setdiff(
      biomarkers_with_data,
      translation_dict_biomarkers
    )
    if (length(in_data_but_not_in_trans) > 0) {
      msg <- paste0(
        "The following biomarkers were retrieved from score sheets' tab names",
        " but have no matching biomarker name in the translation/consolidation rules: ",
        paste0(in_data_but_not_in_trans, collapse = ","),
        ". Biomarkers in translation/consolidation rules: ",
        paste0(translation_dict_biomarkers, collapse = ", "),
        ". Issue comes from the tma_dir: '",
        tma_dir,
        "'"
      )
      cli::cli_abort(msg)
    }
    invalid_biomarkers <- stringr::str_subset(
      biomarkers_with_data,
      "\\.c\\d+"
    )
    if (length(invalid_biomarkers) > 0) {
      msg <- paste0(
        "The following biomarkers have invalid names: ",
        paste0(invalid_biomarkers, collapse = ", "),
        ". Issue comes from the tma_dir: '",
        tma_dir,
        "'. Biomarker names must not match the pattern: ",
        "anything-dot-c-digit (e.g., something.c1, foo.c2, bar.c3)"
      )
      cli::cli_abort(msg)
    }

    # deconvolute combined TMA dataset
    deconvolute(
      tma_file = file.path(
        output_dir,
        .combined_tma_file
      ),
      metadata = metadata,
      output_file = file.path(
        output_dir,
        .deconvoluted_tma_file
      ),
      tma_id = basename(tma_dir)
    )
    # translate numerical biomarker scores to nominal scores
    translate_scores(
      biomarkers_file = file.path(
        output_dir,
        .deconvoluted_tma_file
      ),
      output_file = file.path(
        output_dir,
        .translated_tma_file
      ),
      biomarker_rules_file = biomarker_rules_file
    )
    # and consolidate them for each case
    consolidated_data <- consolidate_scores(
      biomarkers_file = file.path(
        output_dir,
        .translated_tma_file
      ),
      output_file = file.path(
        output_dir,
        .consolidated_tma_file
      ),
      biomarker_rules_file = biomarker_rules_file
    )

    all_spreadsheets[[basename(tma_dir)]] <- consolidated_data
  }

  all_spreadsheets <- dplyr::bind_rows(all_spreadsheets)
  ordered_cols <- unique(
    c("accession_id", "tma_id", "core_id", sort(metadata_columns))
  )
  ordered_cols <- c(
    ordered_cols,
    sort(
      setdiff(colnames(all_spreadsheets), ordered_cols)
    )
  )
  all_spreadsheets <- all_spreadsheets[, ordered_cols, drop = FALSE] |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(metadata_columns),
        ~ dplyr::if_else(is.na(.), "Unk", .)
      )
    )

  if (length(tma_dirs) == 1) {
    # if only 1 tma (one folder being processed)
    writexl::write_xlsx(
      all_spreadsheets,
      path = file.path(output_dir, final_tma_file),
      col_names = TRUE
    )
    return(all_spreadsheets)
  }

  # if multiple tmas
  replicated_acc_id <- all_spreadsheets |>
    dplyr::count(accession_id) |>
    dplyr::filter(n > 1) |>
    dplyr::pull(accession_id)

  if (length(replicated_acc_id) == 0) {
    # no replicated accession IDs
    writexl::write_xlsx(
      all_spreadsheets,
      path = file.path(output_dir, final_tma_file),
      col_names = TRUE
    )
    return(all_spreadsheets)
  }

  replicated <- all_spreadsheets |>
    dplyr::filter(accession_id %in% replicated_acc_id) |>
    dplyr::arrange(accession_id)
  replicated_tmas <- unique(
    all_spreadsheets$tma_id[
      all_spreadsheets$accession_id %in% replicated_acc_id
    ]
  )
  cli::cli_h1("Reconsolidation step")
  cli::cli_alert_info(
    paste0(
      "The following accession IDs are replicated across different TMAs: ",
      paste0(replicated_acc_id, collapse = ", "),
      ". The replicates come from the following TMAs: ",
      paste0(replicated_tmas, collapse = ", "),
      ". {.strong Repeated cases will be reconsolidated}."
    )
  )
  non_biomarker_columns <- c(
    "accession_id",
    "tma_id",
    "core_id",
    metadata_columns
  )
  de_replicated <- replicated |>
    dplyr::select(
      dplyr::all_of(non_biomarker_columns),
      dplyr::matches("\\.c\\d+"),
      -dplyr::matches("\\.c0")
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::matches("\\.c\\d+"),
      names_to = "biomarker_core"
    ) |>
    dplyr::mutate(
      biomarker = stringr::str_remove(biomarker_core, "\\.c\\d+")
    ) |>
    dplyr::group_by(
      accession_id,
      biomarker
    ) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(
      biomarker_core = paste0(
        biomarker,
        ".c",
        dplyr::row_number()
      ),
      dplyr::across(
        dplyr::all_of(setdiff(non_biomarker_columns, "accession_id")),
        function(x) paste0(unique(x), collapse = ";")
      )
    ) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(non_biomarker_columns),
      names_from = biomarker_core,
      values_from = value
    )

  de_replicated_reconsolidated <- consolidate_scores(
    biomarkers_data = de_replicated,
    biomarker_rules_file = biomarker_rules_file,
    late_na_ok = TRUE
  )

  reconsolidated_all <- dplyr::bind_rows(
    all_spreadsheets |>
      dplyr::filter(!(accession_id %in% replicated_acc_id)),
    de_replicated_reconsolidated
  )

  # recompute ordered columns
  ordered_cols <- unique(
    c("accession_id", "tma_id", "core_id", sort(metadata_columns))
  )
  ordered_cols <- c(
    ordered_cols,
    sort(
      setdiff(colnames(reconsolidated_all), ordered_cols)
    )
  )

  reconsolidated_all <- reconsolidated_all[, ordered_cols, drop = FALSE]

  writexl::write_xlsx(
    reconsolidated_all,
    path = file.path(output_dir, final_tma_file),
    col_names = TRUE
  )

  return(reconsolidated_all)
}

#' @title Deconvolute a combined TMA spreadsheet
#' @param tma_file Path to input Excel spreadsheet.
#' Both .xlsx and .xls formats are supported.
#' Must contain one "TMA map" sheet and one or more
#' biomarker-specific sheets (eg, output file of `combine_tma_spreadsheets()`).
#' @param output_file Optional path to output
#' Excel spreadsheet, which can be used as input to `translate_scores()`.
#' @param metadata Optional `data.frame` with at least `core_id` and `accession_id` columns,
#' plus any other metadata columns that should be added to the output 
#' (eg, patient age, sex, etc.).
#' @param tma_id Optional TMA identifier (column `tma_id` will be added to output).
#' @description Reads TMA spreadsheet from `tma_file`
#' which must contain one "TMA map" sheet and one or more
#' biomarker-specific sheets.
#' @details 
#' The function will
#' match core IDs from the TMA map sheet
#' with the biomarker-specific sheets and return a
#' data frame with a "core_id" column and as many
#' columns for each biomarker as necessary.
#'
#' For instance, if core ID 1 has 3 values for biomarker A,
#' the output will contain 3 columns for biomarker A
#' (A.c1, A.c2, A.c3); if another core ID has 2 values for
#' biomarker A, its corresponding A.c1 and A.c2 columns will
#' be filled with the values for that core ID and the
#' A.c3 column will be filled with `NA`.
#'
#' If there are no values for a given core ID in a
#' biomarker-specific sheet, the corresponding
#' columns will all be filled with `NA`.
#' @return A data frame with the deconvoluted data.
#' @export
#' @examples
#' library(TMAtools)
#' # grab folder with example TMA datasets
#' tma_dir <- system.file("extdata", "tma1", package = "TMAtools")
#' # define output files
#' combined_tma_file <- "combined_tma.xlsx"
#' deconvoluted_tma_file <- "deconvoluted_tma.xlsx"
#'
#' # combine TMA datasets
#' combine_tma_spreadsheets(
#'  tma_dir = tma_dir,
#'  output_file = combined_tma_file,
#'  biomarker_sheet_index = 2,
#'  valid_biomarkers = c("ER", "p53") # optional, but recommended to avoid misspelling errors
#' )
#'
#' # deconvolute combined TMA dataset
#' deconvoluted_data <- deconvolute(
#'    tma_file = combined_tma_file,
#'    output_file = deconvoluted_tma_file
#' )
#' print(deconvoluted_data)
deconvolute <- function(
  tma_file,
  metadata = NULL,
  output_file = NULL,
  tma_id = NULL
) {
  sheet_names <- readxl::excel_sheets(tma_file)
  if (!"TMA map" %in% sheet_names) {
    cli::cli_abort("The input spreadsheet must contain a 'TMA map' sheet.")
  }
  spreadsheets <- lapply(
    readxl::excel_sheets(tma_file),
    readxl::read_excel,
    path = tma_file,
    col_types = "text",
    col_names = FALSE,
    trim_ws = FALSE,
    range = readxl::cell_limits(c(1, 1), c(NA, NA)),
    .name_repair = "minimal"
  )
  names(spreadsheets) <- sheet_names
  tma_map <- as.matrix(spreadsheets[["TMA map"]])
  biomarker_names <- sheet_names[sheet_names != "TMA map"]

  n_grid <- nrow(tma_map) * ncol(tma_map)
  core_flat <- as.character(unlist(tma_map))
  pos_df <- data.frame(
    pos = seq_len(n_grid),
    core_id = core_flat,
    stringsAsFactors = FALSE
  )
  pos_df <- pos_df[!is.na(pos_df$core_id), , drop = FALSE]

  bm_list <- lapply(biomarker_names, function(b) {
    data.frame(
      pos = seq_len(n_grid),
      biomarker_name = b,
      value = as.character(unlist(spreadsheets[[b]][
        seq_len(nrow(tma_map)), seq_len(ncol(tma_map))
      ])),
      stringsAsFactors = FALSE
    )
  })
  bm_long <- dplyr::bind_rows(bm_list)

  all_core_ids <- unique(pos_df$core_id)

  merged <- merge(pos_df, bm_long, by = "pos", all = FALSE)
  merged <- merged[, setdiff(colnames(merged), "pos"), drop = FALSE]

  merged <- merged |>
    dplyr::group_by(core_id, biomarker_name) |>
    dplyr::mutate(ck = paste0("c", dplyr::row_number())) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      id_cols = core_id,
      names_from = c(biomarker_name, ck),
      values_from = value,
      names_sep = "."
    )

  first_columns <- "core_id"
  if (!is.null(tma_id)) {
    merged$tma_id <- tma_id
    first_columns <- c(first_columns, "tma_id")
  }

  missing_ids <- setdiff(all_core_ids, merged$core_id)
  if (length(missing_ids) > 0L) {
    missing_df <- as.data.frame(
      matrix(NA_character_, nrow = length(missing_ids), ncol = ncol(merged)),
      stringsAsFactors = FALSE
    )
    colnames(missing_df) <- colnames(merged)
    missing_df$core_id <- as.character(missing_ids)
    if (!is.null(tma_id)) missing_df$tma_id <- tma_id
    merged <- dplyr::bind_rows(merged, missing_df)
  }

  col_names <- colnames(merged)
  ordered_cols <- c(
    first_columns,
    unlist(lapply(biomarker_names, function(b) {
      sort(grep(paste0("^", b, "\\.c[0-9]+$"), col_names, value = TRUE))
    }))
  )

  merged <- merged[, ordered_cols, drop = FALSE]

  if (!is.null(metadata)) {
    metadata$core_id <- as.character(metadata$core_id)
    merged <- dplyr::left_join(
      merged,
      metadata |> dplyr::select(core_id, accession_id, dplyr::everything()),
      by = "core_id"
    ) |>
      dplyr::select(
        accession_id,
        dplyr::any_of(colnames(metadata)),
        dplyr::all_of(ordered_cols)
      )
  }

  if (!is.null(output_file)) {
    writexl::write_xlsx(merged, path = output_file, col_names = TRUE)
    return(invisible(merged))
  }
  return(invisible(merged))
}

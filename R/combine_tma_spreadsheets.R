#' @title Combine TMA spreadsheets
#' @description
#' This function combines multiple TMA spreadsheets into a single spreadsheet
#' that can be used for deconvolution with `TMAtools::deconvolute()`.
#' @param tma_dir The directory containing the TMA spreadsheets. This directory
#' must contain:
#' - One or more excel files for the scores of some biomarker (s)
#' - One excel file with "metadata" in the file name
#' - One excel file with "clean_tma" in the file name
#' @param output_file The name of the output file.
#' @param biomarker_sheet_index The index of the sheet used for ALL files for the biomarker scores.
#' @param required_biomarkers Vector of required biomarkers (optional, just for checking).
#' @return List of data frames (invisible).
#' @export
#' @examples
#' library(TMAtools)
#' tma_dir <- system.file("extdata", package = "TMAtools")
#' combine_tma_spreadsheets(
#'     tma_dir = tma_dir,
#'     output_file = "combined_tma_spreadsheet.xlsx",
#'     biomarker_sheet_index = 2
#' )
#' # check the output Excel file, which should then be used as
#' # the input for `TMAtools::deconvolute()`
combine_tma_spreadsheets <- function(
  tma_dir,
  output_file = "combined_tma_spreadsheet.xlsx",
  biomarker_sheet_index = 2
) {
  tma_files <- list.files(
    path = tma_dir,
    pattern = "\\.xls[x]?$",
    full.names = TRUE
  )
  # filter out metadata files
  meta_ix <- grepl("metadata", tma_files, ignore.case = TRUE)
  clean_map_ix <- grepl("clean_map", tma_files, ignore.case = TRUE)

  stopifnot("FATAL - missing TMA clean map" = any(clean_map_ix))
  stopifnot("FATAL - missing metadata" = any(meta_ix))
  stopifnot("FATAL - more than one TMA clean map" = sum(clean_map_ix) == 1)
  stopifnot("FATAL - more than one metadata" = sum(meta_ix) == 1)

  tma_map_file <- tma_files[clean_map_ix]

  tma_files <- tma_files[!meta_ix & !clean_map_ix]

  stopifnot("FATAL - missing score sheets" = length(tma_files) > 0)

  biomarker_sheet_names <- sapply(
    tma_files,
    function(file) {
      readxl::excel_sheets(file)[biomarker_sheet_index]
    }
  )

  # Loop through each file and read the fourth tab (score sheet)
  biomarker_data_list <- setNames(
    vector("list", length(tma_files)),
    biomarker_sheet_names
  )
  for (i in seq_along(tma_files)) {
    file_path <- tma_files[[i]]
    biomarker_sheet_name <- biomarker_sheet_names[[i]]

    # Read the biomarker sheet
    biomarker_data_list[[biomarker_sheet_name]] <- readxl::read_excel(
      file_path,
      sheet = biomarker_sheet_name,
      trim_ws = FALSE,
      col_names = FALSE,
      col_types = "text",
      .name_repair = "minimal"
    )
  }

  tma_map_list <- list(
    "TMA map" = readxl::read_excel(
      tma_map_file,
      sheet = 1,
      trim_ws = FALSE,
      col_names = FALSE,
      col_types = "text",
      .name_repair = "minimal",
      range = readxl::cell_limits(c(1, 1), c(NA, NA))
    )
  )

  data_list <- c(tma_map_list, biomarker_data_list)

  if (!is.null(output_file)) {
    writexl::write_xlsx(
      data_list,
      output_file,
      col_names = FALSE,
      format_headers = FALSE
    )
  }

  return(invisible(data_list))
}

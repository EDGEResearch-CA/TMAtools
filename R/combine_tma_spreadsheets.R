#' @title Combine TMA spreadsheets
#' @description 
#' This function combines multiple TMA spreadsheets into a single spreadsheet
#' that can be used for deconvolution with `TMAtools::deconvolute()`.
#' @param tma_dir The directory containing the TMA spreadsheets.
#' @param output_file The name of the output file.
#' @param biomarker_sheet_index The index of the sheet used for ALL files for the biomarker scores.
#' @param tma_map_sheet_index The index of the sheet used for ALL files for the TMA map.
#' @return List of data frames (invisible).
#' @export
#' @examples
#' library(TMAtools)
#' tma_dir <- system.file("extdata", package = "TMAtools")
#' combine_tma_spreadsheets(
#'     tma_dir = tma_dir,
#'     output_file = "combined_tma_spreadsheet.xlsx",
#'     biomarker_sheet_index = 2,
#'     tma_map_sheet_index = 1
#' )
#' # check the output Excel file, which should then be used as
#' # the input for `TMAtools::deconvolute()`
combine_tma_spreadsheets <- function(
    tma_dir,
    output_file = "combined_tma_spreadsheet.xlsx",
    biomarker_sheet_index = 2,
    tma_map_sheet_index = 1
) {
    tma_files <- list.files(
        path = tma_dir,
        pattern = "\\.xls[x]?$",
        full.names = TRUE
    )

    biomarker_sheet_names <- sapply(
        tma_files,
        function(file) {
            readxl::excel_sheets(file)[biomarker_sheet_index]
        }
    )
    tma_map_sheet_names <- sapply(
        tma_files,
        function(file) {
            readxl::excel_sheets(file)[tma_map_sheet_index]
        }
    )

    # Loop through each file and read the fourth tab (score sheet)
    biomarker_data_list <- setNames(
        vector("list", length(tma_files)),
        biomarker_sheet_names
    )
    tma_map_data_list <- vector("list", length(tma_files))
    for (i in seq_along(tma_files)) {
        file_path <- tma_files[[i]]
        biomarker_sheet_name <- biomarker_sheet_names[[i]]
        tma_map_sheet_name <- tma_map_sheet_names[[i]]
        
        # Read the biomarker sheet
        biomarker_data_list[[biomarker_sheet_name]] <- readxl::read_excel(
            file_path,
            sheet = biomarker_sheet_name,
            trim_ws = FALSE,
            col_names = FALSE,
            col_types = "text",
            .name_repair = "minimal"
        )
        # Read the TMA map sheet
        tma_map_data_list[[i]] <- readxl::read_excel(
            file_path,
            sheet = tma_map_sheet_name,
            trim_ws = FALSE,
            col_names = FALSE,
            col_types = "text",
            .name_repair = "minimal"
        )
        
    }

    data_list <- c(
        list("TMA map" = tma_map_data_list[[1]]),
        biomarker_data_list
    )
    
    writexl::write_xlsx(
        data_list,
        output_file,
        col_names = FALSE,
        format_headers = FALSE
    )

    return(invisible(data_list))
}
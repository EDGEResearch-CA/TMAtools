#' @title Deconvolute TMA map
#' @param tma_file Path to input Excel spreadsheet.
#' Both .xlsx and .xls formats are supported.
#' Must contain one "TMA map" sheet and one or more
#' biomarker-specific sheets.
#' @param output_file Optional path to output
#' Excel spreadsheet.
#' @description Reads TMA spreadsheet from `tma_file`
#' which must contain one "TMA map" sheet and one or more
#' biomaker-specific sheets.
#'
#' @return A data frame with the deconvoluted data.
#' The function will
#' match core IDs from the TMA map sheet
#' with the biomarker-specific sheets and return a
#' data frame with a "core_id" column and as many
#' columns for each biomarker as necessary.
#'
#' For instance, if core ID 1 has 3 values for biomarker A,
#' the output will contain 3 columns for biomarker A
#' (A.c1, A.c2, A.c3); if another core ID has 2 values for
#' biomaker A, its corresponding A.c1 and A.c2 columns will
#' be filled with the values for that core ID and the
#' A.c3 column will be filled with NA.
#'
#' If there are no values for a given core ID in a
#' biomarker-specific sheet, the corresponding
#' columns will all be filled with NA.
#' @export
#' @examples
#' library(TMAtools)
#' tma_file <- system.file("extdata", "example.xlsx", package = "TMAtools")
#' deconvoluted_data <- deconvolute(tma_file)
#' head(deconvoluted_data)
deconvolute <- function(tma_file, output_file = NULL) {
    sheet_names <- readxl::excel_sheets(tma_file)
    if (!"TMA map" %in% sheet_names) {
        stop("The input spreadsheet must contain a 'TMA map' sheet.")
    }
    spreadsheets <- lapply(
        readxl::excel_sheets(tma_file),
        readxl::read_excel,
        path = tma_file,
        col_types = "text",
        col_names = FALSE,
        .name_repair = "minimal"
    )
    names(spreadsheets) <- sheet_names
    tma_map <- spreadsheets[["TMA map"]]
    matches_tma_map <- sapply(
        sheet_names,
        function(sheet_name) {
            if (sheet_name == "TMA map") {
                return(TRUE)
            }
            .d <- spreadsheets[[sheet_name]]
            all.equal(which(is.na(.d)), which(is.na(tma_map)))
        }
    )
    if (!all(matches_tma_map)) {
        msg <- paste0(
            "The non-empty cells in the TMA map sheet and the ",
            "biomarker-specific sheets do not match for the following sheets:\n",
            paste0(sheet_names[!matches_tma_map], collapse = ", ")
        )
        stop(msg)
    }
    core_ids <- unique(na.omit(unlist(tma_map)))
    biomarker_names <- sheet_names[sheet_names != "TMA map"]
    results <- setNames(
        lapply(vector("list", length(core_ids)), \(...) list()),
        core_ids
    )
    for (core_id in core_ids) {
        core_ix <- !is.na(tma_map) & tma_map == core_id
        for (biomarker_name in biomarker_names) {
            biomarker_values <- spreadsheets[[biomarker_name]][core_ix]
            if (length(biomarker_values) == 0L) {
                biomarker_values <- c("c1" = NA)
            } else {
                names(biomarker_values) <- paste0(
                    "c",
                    seq_along(biomarker_values)
                )
            }
            results[[core_id]][[biomarker_name]] <- list(biomarker_values)
        }
    }
    deconvoluted_results <- dplyr::bind_rows(results, .id = "core_id") |>
        tidyr::pivot_longer(
            -core_id,
            names_to = "biomarker_name",
            values_to = "value"
        ) |>
        tidyr::unnest_wider(value) |>
        tidyr::pivot_wider(
            names_from = "biomarker_name",
            values_from = dplyr::matches("^c[0-9]+$"),
            names_glue = "{biomarker_name}.{.value}",
        )
    col_names <- colnames(deconvoluted_results)
    ordered_cols <- c(
        "core_id",
        unlist(
            lapply(
                biomarker_names,
                function(b) {
                    sort(grep(
                        paste0("^", b, "\\.c[0-9]+$"),
                        col_names,
                        value = TRUE
                    ))
                }
            )
        )
    )

    deconvoluted_results <- deconvoluted_results[, ordered_cols]

    if (!is.null(output_file)) {
        writexl::write_xlsx(
            deconvoluted_results,
            path = output_file,
            col_names = TRUE
        )
        return(invisible(deconvoluted_results))
    }
    return(invisible(deconvoluted_results))
}

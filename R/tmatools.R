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
#' @param biomarker_sheet_index The index of the biomarker sheet in the TMA file.
#' @param tma_map_sheet_index The index of the TMA map sheet in the TMA file.
#' @param required_biomarkers A character vector of required biomarkers.
#' @return NULL
#' @export
#' @examples
#' library(TMAtools)
#' tma_dirs <- c(
#'  system.file("extdata", "tma1", package = "TMAtools"),
#'  system.file("extdata", "tma2", package = "TMAtools")
#' )
#' # Run the TMAtools pipeline
#' tmatools(
#'   tma_dirs = tma_dirs,
#'   output_dir = "tmatools_output"
#' )
tmatools <- function(
    tma_dirs,
    output_dir = "tmatools_output",
    combined_tma_file = "combined_tma.xlsx",
    deconvoluted_tma_file = "deconvoluted_tma.xlsx",
    consolidated_tma_file = "consolidated_tma.xlsx",
    biomarker_sheet_index = 2,
    tma_map_sheet_index = 1,
    required_biomarkers = c("ER", "TP53")
) {
    for (tma_dir in tma_dirs) {
        if (!dir.exists(tma_dir)) {
            stop("Directory does not exist: ", tma_dir)
        }
    }
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    combined_prefix <- tools::file_path_sans_ext(
        basename(combined_tma_file)
    )
    deconvoluted_prefix <- tools::file_path_sans_ext(
        basename(deconvoluted_tma_file)
    )
    consolidated_prefix <- tools::file_path_sans_ext(
        basename(consolidated_tma_file)
    )
    # run TMAtools pipeline for each TMA directory
    all_spreadsheets <- setNames(
        vector("list", length(tma_dirs)),
        basename(tma_dirs)
    )
    for (tma_dir in tma_dirs) {
        metadata_file <- list.files(
            path = tma_dir,
            pattern = "*metadata*",
            full.names = TRUE
        )
        if (length(metadata_file) == 0) {
            stop("No metadata file found in: ", tma_dir)
        }
        if (length(metadata_file) > 1) {
            stop("Multiple metadata files found in: ", tma_dir)
        }
        metadata <- readxl::read_excel(metadata_file[1], sheet = 1)
        if (!all(c("core_id", "accession_id") %in% colnames(metadata))) {
            stop(
                "Metadata file does not contain required columns: core_id, accession_id"
            )
        }

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
            tma_map_sheet_index = tma_map_sheet_index
        )
        # deconvolute combined TMA dataset
        deconvolute(
            tma_file = file.path(
                output_dir,
                .combined_tma_file
            ),
            output_file = file.path(
                output_dir,
                .deconvoluted_tma_file
            )
        )
        # translate numerical biomarker scores to nominal scores
        # and consolidate them for each case
        consolidated_data <- translate_and_consolidate_scores(
            biomarkers_file = file.path(
                output_dir,
                .deconvoluted_tma_file
            ),
            required_biomarkers = required_biomarkers,
            output_file = file.path(
                output_dir,
                .consolidated_tma_file
            )
        )

        all_spreadsheets[[basename(tma_dir)]] <- dplyr::left_join(
            consolidated_data,
            metadata,
            by = "core_id"
        )
    }

    all_spreadsheets <- dplyr::bind_rows(
        all_spreadsheets,
        .id = "tma_id"
    ) |>
        dplyr::select(
            tma_id,
            core_id,
            accession_id,
            dplyr::everything()
        )
    replicated_acc_id <- all_spreadsheets |>
        dplyr::count(accession_id) |>
        dplyr::filter(n > 1) |>
        dplyr::pull(accession_id)
    if (length(replicated_acc_id) == 0) {
        # no replicated accession IDs
        return(all_spreadsheets)
    }

    message(
        "The following accession IDs are replicated across different TMAs: ",
        paste(replicated_acc_id, collapse = ", ")
    )

    replicated <- all_spreadsheets |>
        dplyr::filter(accession_id %in% replicated_acc_id)
    replicated_tmas <- unique(
        all_spreadsheets$tma_id[
            all_spreadsheets$accession_id %in% replicated_acc_id
        ]
    )
    replicated_tma_dirs <- tma_dirs[
        basename(tma_dirs) %in% replicated_tmas
    ]
    de_replicated <- replicated |>
        dplyr::select(
            tma_id,
            accession_id,
            dplyr::matches("\\.c\\d+")
        ) |>
        tidyr::pivot_longer(
            cols = dplyr::matches("\\.c\\d+"),
            names_to = "biomarker_core"
        ) |>
        dplyr::mutate(
            biomarker = gsub("\\.c\\d+", "", biomarker_core)
        ) |>
        dplyr::group_by(
            accession_id,
            biomarker
        ) |>
        dplyr::mutate(
            new_biomarker_core = paste0(
                biomarker,
                ".c",
                dplyr::row_number()
            )
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(
            id_cols = accession_id,
            names_from = new_biomarker_core,
            values_from = value
        )

    no_replicated <- all_spreadsheets |>
        dplyr::filter(!(accession_id %in% replicated_acc_id))

    return(list(
        replicated = de_replicated,
        unique = no_replicated
    ))
}

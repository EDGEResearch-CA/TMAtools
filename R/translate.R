#' @title Translate biomarker scores from numerical to nominal
#' @description This function translates numerical scores of biomarkers to nominal scores.
#' @param biomarkers_file Path to the Excel file containing biomarker data.
#' @param biomarker_rules_file Path to spreadsheet containing the translation rules for all biomarkers.
#' It must contain a sheet named "translation" with columns
#' "biomarker", "original_score", "translated_score".
#' @param output_file Optional path to the output file. If NULL, the function will not save the output.
#' @return A data frame with translated biomarker scores.
#' @export
#' @examples
#' library(TMAtools)
#' # grab folder with example TMA datasets
#' tma_dir <- system.file("extdata", "tma1", package = "TMAtools")
#' # define output files
#' combined_tma_file <- "combined_tma.xlsx"
#' deconvoluted_tma_file <- "deconvoluted_tma.xlsx"
#' consolidated_tma_file <- "consolidated_tma.xlsx"
#'
#' # combine TMA datasets
#' combine_tma_spreadsheets(
#'  tma_dir = tma_dir,
#'  output_file = combined_tma_file
#' )
#'
#' # deconvolute combined TMA dataset
#' deconvolute(
#'    tma_file = combined_tma_file,
#'    output_file = deconvoluted_tma_file
#' )
#'
#' # translate numerical biomarker scores to nominal scores
#' # and consolidate them for each case
#' consolidated_data <- translate_and_consolidate_scores(
#'   biomarkers_file = deconvoluted_tma_file,
#'   required_biomarkers = c("ER", "TP53")
#' )
#' head(consolidated_data)
translate_scores <- function(
  biomarkers_file,
  biomarker_rules_file = NULL,
  output_file = NULL
) {
  ## Define required biomarkers (17) for analyses 2025
  translation_dict <- get_translation_dictionary(
    biomarker_rules_file = biomarker_rules_file
  )

  required_biomarkers <- names(translation_dict)

  # Harmonization of input file ----
  ## read biomarker data for a TMA (deconvoluted numerical scores)
  biomarkers_data <- readxl::read_excel(
    biomarkers_file,
    col_types = "text"
  )

  ## replacing all potential blank spaces (NA when imported to R) with "x" - which will be replaced with Unknown after
  biomarkers_data <- biomarkers_data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ dplyr::if_else(is.na(.), "x", .)
      )
    )

  ## ensure all required biomarkers have at least one column in biomarkers_data
  # if a required biomarker is missing a placeholder column will be created and named as biomarker.c0

  invalid_scores <- list()
  for (biomarker_name in required_biomarkers) {
    does_not_contain_biomarker_name <- !any(
      stringr::str_detect(
        colnames(biomarkers_data),
        stringr::fixed(biomarker_name)
      )
    ) # does not matter if the biomarker name in the columns is uppercase or lowercase

    if (does_not_contain_biomarker_name) {
      message(
        paste0(
          "Adding placeholder column for biomarker ",
          biomarker_name
        )
      )
      biomarkers_data[[paste0(biomarker_name, ".c0")]] <- "x"
    }
    # check if all TMA cores are covered by the dictionary
    biomarker_columns <- colnames(biomarkers_data)[
      stringr::str_detect(
        colnames(biomarkers_data),
        paste0(biomarker_name, "\\.c\\d+")
      )
    ]
    unique_scores <- unique(unlist(biomarkers_data[, biomarker_columns]))
    .invalid_scores <- setdiff(
      unique_scores,
      names(translation_dict[[biomarker_name]])
    )
    if (length(.invalid_scores) > 0) {
      invalid_scores[[biomarker_name]] <- .invalid_scores
    }
  }

  if (length(invalid_scores) > 0) {
    msg <- "There are biomarkers with scores not covered in the translation dictionary: "
    for (i in seq_along(invalid_scores)) {
      invalid_biomarker_name <- names(invalid_scores)[i]
      msg <- paste0(
        msg,
        invalid_biomarker_name,
        " (",
        paste0(invalid_scores[[invalid_biomarker_name]], collapse = ", "),
        ifelse(i < length(invalid_scores), "); ", ")")
      )
    }
    cli::cli_abort(msg)
  }

  # Replacing numerical scores with nominal scores for each biomarker ----
  for (biomarker_name in required_biomarkers) {
    biomarkers_data <- biomarkers_data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::matches(paste0(biomarker_name, "\\.c\\d+")),
          ~ translation_dict[[biomarker_name]][as.character(.x)]
        )
      )
  } # only cores will be consolidated when the format of the column name is: biomarker name.c(any number)
  # if the column name corresponds to the biomaker name only the biomaker is already consolidated

  ## SAFETY CHECK for translation of numerical scores ----
  # check for any NA's in nominal scores
  # if any NA it is probably some numerical score that is not covered in the dictionary

  if (any(!complete.cases(biomarkers_data))) {
    biomarkers_data_with_na <- biomarkers_data[
      !complete.cases(biomarkers_data),
    ]
    print(biomarkers_data_with_na)
    cli::cli_abort(
      "The cases above contain some NA after replacing numerical scores with nominal scores."
    )
  }

  if (!is.null(output_file)) {
    writexl::write_xlsx(
      biomarkers_data,
      output_file
    )
  }

  return(invisible(biomarkers_data))
}

#' @title Get translation dictionary from biomarker rules file
#' @param biomarker_rules_file Path to spreadsheet containing the translation rules for all biomarkers.
#' It must contain a sheet named "translation" with columns
#' "biomarker", "original_score", "translated_score".
#' @return data.frame with the translation rules.
#' @export
get_translation_dictionary <- function(biomarker_rules_file) {
  if (!file.exists(biomarker_rules_file)) {
    cli::cli_abort(paste0(
      "Biomarker rules file ",
      biomarker_rules_file,
      " does not exist."
    ))
  }
  dict_df <- suppressMessages({
    readxl::read_excel(
      path = biomarker_rules_file,
      sheet = "translation",
      col_types = "text"
    )
  })
  required_columns <- c("biomarker", "original_score", "translated_score")
  missing_cols <- setdiff(
    c("biomarker", "original_score", "translated_score"),
    colnames(dict_df)
  )
  if (length(missing_cols) > 0) {
    cli::cli_abort(paste0(
      "Biomarker rules file ",
      biomarker_rules_file,
      " is missing columns required for translation: ",
      paste0(missing_cols, collapse = ", ")
    ))
  }

  # select only required columns
  dict_df <- dict_df[, required_columns]

  .dict <- lapply(
    split(dict_df, dict_df$biomarker),
    function(biomarker_sub_df) {
      setNames(
        biomarker_sub_df$translated_score,
        biomarker_sub_df$original_score
      )
    }
  )
  return(.dict)
}

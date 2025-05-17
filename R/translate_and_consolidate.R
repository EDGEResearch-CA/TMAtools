#' @title Consolidate biomarker scores
#' @description This function translates numerical scores of biomarkers to nominal scores and consolidates them for each case.
#' @param biomarkers_file Path to the Excel file containing biomarker data.
#' @param required_biomarkers A vector of required biomarkers. If NULL, default biomarkers are used.
#' @param get_dict A function that returns a dictionary for translating numerical scores to nominal scores. If NULL, a default function is used.
#' @return A data frame with consolidated biomarker scores and MMR status.
#' @seealso \code{\link{get_leopard_dict}} for the default dictionary function.
#' @export
#' @examples
#' library(TMAtools)
#' # grab folder with example TMA datasets
#' tmadir <- system.file("extdata", "tmadir", package = "TMAtools")
#' # define output files
#' combined_tma_file <- "combined_tma.xlsx"
#' deconvoluted_tma_file <- "deconvoluted_tma.xlsx"
#' consolidated_tma_file <- "consolidated_tma.xlsx"
#'
#' # combine TMA datasets
#' combine_tma_spreadsheets(
#'  tma_dir = tmadir,
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
translate_and_consolidate_scores <- function(
  biomarkers_file,
  required_biomarkers = NULL,
  get_dict = NULL
) {
  ## Define required biomarkers (17) for analyses 2025

  if (is.null(required_biomarkers)) {
    required_biomarkers <- c(
      "ER",
      "PR",
      "WT1",
      "PAX2",
      "GATA3",
      "TTF1",
      "CDX2",
      "MLH1",
      "MSH2",
      "MSH6",
      "PMS2",
      "ARID1A",
      "RB1",
      "CTNNB1",
      "TP53",
      "p16",
      "PTEN"
    )
  }

  ## Dictionary of biomarkers to replace numerical scores with nominal scores ----
  # MK scoring system reviewed in 2025
  if (is.null(get_dict) || !is.function(get_dict)) {
    get_dict <- get_leopard_dict
  }

  # Harmonization of input file ----
  ## read biomarker data for a TMA (deconvoluted numerical scores)
  biomarkers_data <- readxl::read_excel(
    biomarkers_file,
    col_types = "text"
  )

  ## replacing all potential blank spaces (NA when imported to R) with "x" - which will be replaced with Unknown after
  biomarkers_data <- biomarkers_data |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ ifelse(is.na(.), "x", .)
    ))

  ## ensure all required biomarkers have at least one column in biomarkers_data
  # if a required biomarker is missing a placeholder column will be created and named as biomarker.c0

  for (biomarker_name in required_biomarkers) {
    doest_not_contain_biomarker_name <- !any(
      stringr::str_detect(
        colnames(biomarkers_data),
        stringr::fixed(biomarker_name, ignore_case = TRUE)
      )
    ) # does not matter if the biomarker name in the columns is uppercase or lowercase

    if (doest_not_contain_biomarker_name) {
      message(paste0(
        "Adding placeholder column for biomarker ",
        biomarker_name
      ))
      biomarkers_data[[paste0(biomarker_name, ".c0")]] <- "x"
    }
  }

  # Replacing numerical scores with nominal scores for each biomarker ----

  get_nominal_scores <- function(numeric_scores, biomarker_name) {
    numeric_scores <- as.character(numeric_scores)
    numeric_to_nominal_dict <- get_dict(biomarker = biomarker_name)
    nominal_scores <- numeric_to_nominal_dict[numeric_scores]
    return(nominal_scores)
  }

  for (biomarker_name in required_biomarkers) {
    biomarkers_data <- biomarkers_data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::matches(paste0(biomarker_name, "\\.c\\d+")),
          ~ get_nominal_scores(.x, biomarker_name)
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
    stop(
      "The cases above contain some NA after replacing numerical scores with nominal scores."
    )
  }

  # Definition of useful biomarker subgroups for consolidation ----
  # Defining p53 abnormal scores correlated with mutation
  p53_abnormal_scores <- c(
    "complete absence",
    "overexpression",
    "cytoplasmic",
    "abnormal",
    "subclonal"
  )

  # Defining PTEN uninterpretable scores
  PTEN_uninterpretable_scores <- c("Unk", "equivocal blush", "nuclear")

  # Defining MMR (MLH1, MSH2, PMS2, MSH6) scores for absence/loss correlated with deficiency
  MMR_deficient_scores <- c("absent", "subclonal loss")

  # Consolidating biomarker scores for each single case ----
  # This script works regardless the number of TMA cores per biomarker

  consolidate_biomarker <- function(single_row, biomarker_name) {
    # single_row is a vector with the results of the TMA cores for a single case
    # biomarker_name is the specific biomarker you are looking at

    # if all values are Unk, consolidated value must be Unk
    if (all(single_row == "Unk")) {
      return("Unk")
    }

    ## Biomarker-specific dictionary ----
    # each biomarker contains its own specific consolidation rules (MK reviewed 2025)
    if (
      biomarker_name %in% c("WT1", "ER", "PR", "PAX2", "GATA3", "TTF1", "CDX2")
    ) {
      if (any(single_row == "diffuse (>50%)")) {
        consolidated_value <- "diffuse (>50%)"
      } else if (any(single_row == "focal (1-50%)")) {
        consolidated_value <- "focal (1-50%)"
      } else {
        consolidated_value <- "negative"
      }
      return(consolidated_value)
    }

    if (
      biomarker_name %in% c("MLH1", "MSH2", "MSH6", "PMS2", "ARID1A", "RB1")
    ) {
      if (any(single_row %in% MMR_deficient_scores)) {
        consolidated_value <- "absent"
      } else {
        consolidated_value <- "present"
      }
      return(consolidated_value)
    }

    if (biomarker_name == "TP53") {
      if (any(single_row %in% p53_abnormal_scores)) {
        consolidated_value <- "mutated"
      } else {
        consolidated_value <- "wild type"
      }
      return(consolidated_value)
    }

    if (biomarker_name == "CTNNB1") {
      if (any(single_row == "nuclear")) {
        consolidated_value <- "mutated"
      } else if (any(single_row == "cytoplasmic")) {
        consolidated_value <- "cytoplasmic"
      } else if (any(single_row == "membranous")) {
        consolidated_value <- "membranous"
      } else {
        consolidated_value <- "negative"
      }
      return(consolidated_value)
    }

    if (biomarker_name == "p16") {
      if (any(single_row == "normal")) {
        consolidated_value <- "normal"
      } else if (
        "abnormal complete absence" %in%
          single_row &
          "abnormal block" %in% single_row
      ) {
        consolidated_value <- "duo abnormal"
      } else if (any(single_row == "abnormal block")) {
        consolidated_value <- "abnormal block"
      } else {
        consolidated_value <- "abnormal complete absence"
      }
      return(consolidated_value)
    }

    if (biomarker_name == "PTEN") {
      if (any(single_row == "subclonal")) {
        consolidated_value <- "subclonal"
      } else if (
        "absent" %in%
          single_row &
          "reduced" %in% single_row |
          "absent" %in% single_row & "retained" %in% single_row
      ) {
        consolidated_value <- "subclonal"
      } else if (any(single_row == "absent")) {
        consolidated_value <- "absent"
      } else if (any(single_row == "retained")) {
        consolidated_value <- "retained"
      } else if (all(single_row %in% PTEN_uninterpretable_scores)) {
        consolidated_value <- "Unk"
      } else {
        consolidated_value <- "reduced"
      }
      return(consolidated_value)
    }
  }

  ## For loop to consolidate all TMA cores for a required biomarker ----
  for (biomarker_name in required_biomarkers) {
    # getting all the columns of required biomarkers
    biomarker_columns_names <- stringr::str_subset(
      colnames(biomarkers_data),
      stringr::fixed(biomarker_name, ignore_case = TRUE)
    )
    # getting biomarkers that were deconvoluted already

    if (length(biomarker_columns_names) == 0) {
      stop("FATAL - failed in finding all required biomarkers")
    }

    # for pretty printing of the message containing the names of the columns
    biomarker_columns_names_string <- paste0(
      biomarker_columns_names,
      collapse = ", "
    )

    # getting all the nominal scores of all cores for a required biomarker
    biomarker_values <- biomarkers_data[biomarker_columns_names]

    if (any(stringr::str_detect(biomarker_columns_names, "\\.c\\d+"))) {
      # it means there are columns like XX.c1, XX.c2 where XX is the biomarker_name
      message(paste0(
        "Consolidating ",
        biomarker_name,
        " using columns: ",
        biomarker_columns_names_string
      ))
      consolidated_biomaker_value <- apply(
        biomarker_values,
        1,
        function(single_row) {
          consolidate_biomarker(
            single_row = single_row,
            biomarker_name = biomarker_name
          )
        }
      )
    } else {
      # it means there are no columns with .c1, .c2 etc, so biomarker is (probably) consolidated already
      if (length(biomarker_columns_names) > 1) {
        # ensure there is a single column, representing already consolidated data
        stop(paste0(
          "Consolidation failed for biomarker ",
          biomarker_name,
          " with columns: ",
          biomarker_columns_names_string
        ))
      }
      consolidated_biomaker_value <- unname(unlist(biomarker_values))
    }
    biomarkers_data[[stringr::str_to_lower(
      biomarker_name
    )]] <- consolidated_biomaker_value
  }

  # Make output pretty ----
  # reorder columns so that cores are followed by consolidated value
  biomarkers_data <- biomarkers_data |>
    dplyr::select(
      -dplyr::contains(required_biomarkers),
      dplyr::contains(required_biomarkers)
    )

  # select columns of interest only:
  # scores from individuals cores and consolidated values of required biomarkers

  biomarkers_data <- biomarkers_data |>
    dplyr::select(
      1,
      2, # Columns 1 and 2 should correspond to Accession ID and Core ID
      dplyr::contains(required_biomarkers)
    )

  # Assess mismatch repair status ----

  # If no MMR biomarker is absent, MSH6 and PMS2 need to have interpretable values
  mmr_biomarkers <- c("mlh1", "msh2", "msh6", "pms2")
  if (all(mmr_biomarkers %in% colnames(biomarkers_data))) {
    biomarkers_data[["mmr_ihc_4"]] <- apply(
      biomarkers_data,
      1,
      function(single_row) {
        names(single_row) <- stringr::str_to_lower(names(single_row))
        msh6 <- single_row["msh6"]
        pms2 <- single_row["pms2"]
        mlh1 <- single_row["mlh1"]
        msh2 <- single_row["msh2"]
        if (all(c(msh6, pms2, mlh1, msh2) == "Unk")) {
          # if at least one of MSH6, PMS2, MLH1, MSH2 is absent,
          # MMR status is 'deficient'
          return("Unk")
        } else if (any(c(msh6, pms2, mlh1, msh2) == "absent")) {
          # if at least one of MSH6, PMS2, MLH1, MSH2 is absent,
          # MMR status is 'deficient'
          return("deficient")
        } else if (msh6 == "Unk" | pms2 == "Unk") {
          # if MSH6 or PMS2 are unknown, MMR status is 'Unk' (regardless of MLH1 and MSH2)
          # because MSH6 and PMS2 are mandatory to assess proficient status
          return("Unk")
        } else {
          # if MSH6 and PMS2 are known (and present)
          # AND none of MSH6, PMS2, MLH1, MSH2 is absent,
          # MMR status is 'intact'
          return("intact")
        }
      }
    )
  }

  return(biomarkers_data)
}

#' @title Get dictionary for LEOPARD biomarkers (2025)
#' @description This function returns a dictionary for translating numerical scores to nominal scores for LEOPARD biomarkers.
#' @param biomarker The name of the biomarker.
#' @return A named vector representing the dictionary for the specified biomarker.
#' @export
#' @examples
#' library(TMAtools)
#' print(get_leopard_dict("ER"))
#' print(get_leopard_dict("CTNNB1"))
get_leopard_dict <- function(biomarker) {
  if (biomarker %in% c("ER", "PR", "WT1", "PAX2", "GATA3", "TTF1", "CDX2")) {
    dict <- c(
      "0" = "negative",
      "1" = "focal (1-50%)",
      "2" = "diffuse (>50%)",
      "9" = "Unk",
      "x" = "Unk"
    )
  } else if (biomarker %in% c("CTNNB1")) {
    dict <- c(
      "0" = "negative",
      "1" = "membranous",
      "2" = "cytoplasmic",
      "3" = "nuclear",
      "9" = "Unk",
      "x" = "Unk"
    )
  } else if (
    biomarker %in% c("MLH1", "MSH2", "MSH6", "PMS2", "ARID1A", "RB1")
  ) {
    dict <- c(
      "0" = "absent",
      "1" = "present",
      "2" = "subclonal loss",
      "3" = "Unk",
      "8" = "Unk",
      "9" = "Unk",
      "x" = "Unk"
    )
  } else if (biomarker %in% c("TP53")) {
    dict <- c(
      "0" = "complete absence",
      "1" = "wild type",
      "2" = "overexpression",
      "3" = "cytoplasmic",
      "4" = "abnormal",
      "5" = "subclonal",
      "8" = "Unk",
      "9" = "Unk",
      "x" = "Unk"
    )
  } else if (biomarker %in% c("p16")) {
    dict <- c(
      "0" = "abnormal complete absence",
      "1" = "normal",
      "2" = "abnormal block",
      "3" = "Unk",
      "9" = "Unk",
      "x" = "Unk"
    )
  } else if (biomarker %in% c("PTEN")) {
    dict <- c(
      "0" = "absent",
      "1" = "equivocal blush",
      "2" = "reduced",
      "3" = "retained",
      "4" = "subclonal",
      "5" = "nuclear",
      "9" = "Unk",
      "x" = "Unk"
    )
  } else {
    rlang::abort(stringr::str_glue(
      "Don't know dictionary for biomarker={biomarker}"
    ))
  }
  return(dict)
}

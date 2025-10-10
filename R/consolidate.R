#' @title Consolidate biomarkers
#' @description This function consolidates biomarker scores for each case.
#' @param biomarkers_file Path to the Excel file containing biomarker data.
#' @param biomarker_rules_file Path to spreadsheet containing the consolidation rules for all biomarkers.
#' It must contain a sheet named "consolidation" with columns
#' "biomarker", "rule_type", "rule_value", "consolidated_value".
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
#' translated_tma_file <- "translated_tma.xlsx"
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
#' # translate numerical scores to nominal scores
#' translate_scores(
#'    biomarkers_file = deconvoluted_tma_file,
#'    biomarker_rules_file = "tmp/biomarker_rules.xlsx",
#'    output_file = translated_tma_file
#' )
#'
#' # and consolidate nominal scores for each case
#' consolidated_data <- translate_and_consolidate_scores(
#'   biomarkers_file = deconvoluted_tma_file,
#'   required_biomarkers = c("ER", "TP53")
#' )
#' head(consolidated_data)
consolidate_scores <- function(
  biomarkers_file,
  biomarker_rules_file = NULL,
  output_file = NULL
) {
  consolidation_df <- get_consolidation_rules_df(
    biomarker_rules_file = biomarker_rules_file
  )
  required_biomarkers <- unique(consolidation_df$biomarker)
  required_biomarkers <- required_biomarkers[required_biomarkers != "all"]

  ## read biomarker data for a TMA (deconvoluted numerical scores)
  biomarkers_data <- readxl::read_excel(
    biomarkers_file,
    col_types = "text"
  )

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

  # loop through biomarkers
  for (biomarker_name in required_biomarkers) {
    # rules for current biomarker
    rules_df <- consolidation_df[consolidation_df$biomarker == biomarker_name, ]
    # scores for current biomarker
    biomarker_scores <- biomarkers_data |>
      dplyr::select(
        dplyr::starts_with(
          paste0(biomarker_name, ".c"),
          ignore.case = TRUE
        )
      )
    bname_lower <- stringr::str_to_lower(
      biomarker_name
    )
    biomarkers_data[[bname_lower]] <- apply(
      biomarker_scores,
      1,
      function(scores) {
        consolidate_single_patient(
          rules_df = rules_df,
          scores = unname(scores)
        )
      }
    )
  }

  ## SAFETY CHECK for consolidation ----
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

  # reorder columns so that cores are followed by consolidated value
  biomarkers_data <- biomarkers_data |>
    dplyr::select(
      -dplyr::contains(required_biomarkers),
      dplyr::contains(required_biomarkers)
    )

  if (!is.null(output_file)) {
    writexl::write_xlsx(
      biomarkers_data,
      output_file
    )
  }

  return(invisible(biomarkers_data))
}

#' @title Get consolidation data.frame from biomarker rules file
#' @param biomarker_rules_file Path to spreadsheet containing the consolidation rules for all biomarkers.
#' It must contain a sheet named "consolidation" with columns
#' "biomarker", "rule_type", "rule_value", "consolidated_value".
#' @return a data.frame of consolidation rules for each biomarker.
get_consolidation_rules_df <- function(biomarker_rules_file) {
  if (!file.exists(biomarker_rules_file)) {
    cli::cli_abort(paste0(
      "Biomarker rules file ",
      biomarker_rules_file,
      " does not exist."
    ))
  }
  consolidation_df <- readxl::read_excel(
    path = biomarker_rules_file,
    sheet = "consolidation",
    col_types = c("text", "text", "text", "text")
  )
  missing_cols <- setdiff(
    c("biomarker", "rule_type", "rule_value", "consolidated_value"),
    colnames(consolidation_df)
  )
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      paste0(
        "Biomarker rules file ",
        biomarker_rules_file,
        " is missing columns required for consolidation: ",
        paste0(missing_cols, collapse = ", ")
      )
    )
  }

  # check that all biomarkers have at least one else rule
  # except for "all"
  biomarkers <- unique(consolidation_df$biomarker)
  for (biomarker in biomarkers) {
    if (biomarker != "all") {
      biomarker_sub_df <- consolidation_df[
        consolidation_df$biomarker == biomarker,
      ]
      if (!any(biomarker_sub_df$rule_type == "else")) {
        cli::cli_abort(paste0(
          "Biomarker ",
          biomarker,
          " does not have an 'else' rule. Please add one to ensure all cases are covered."
        ))
      }
    }
  }

  return(consolidation_df)
}

#' @title Assess consolidated score per patient
#' @param rules_df data.frame with consolidation rules for a single biomarker
#' @param scores character vector with all scores for a given biomarker/patient
#' @param unknow_values character vector with values treated as unknown.
#' Defaults to `c("Unk", "x")`.
consolidate_single_patient <- function(
  rules_df,
  scores,
  unknown_values = c("Unk", "x")
) {
  if (any(is.na(scores))) {
    cli::cli_abort("Scores contain NA values.")
  }

  if (all(scores %in% unknown_values)) {
    return("Unk")
  }

  # loop through rule sets (rows of biomarker rules file)
  for (i in seq_len(nrow(rules_df))) {
    # grab rules
    rule_types <- stringr::str_split_1(rules_df$rule_type[i], pattern = ";")

    if (length(rule_types) == 1 && rule_types == "else") {
      rule_values <- NA
    } else {
      rule_values <- stringr::str_split_1(rules_df$rule_value[i], pattern = ";")
    }
    consolidated_value <- rules_df$consolidated_value[i]
    n_rules <- length(rule_types)
    if (length(rule_values) != n_rules) {
      cli::cli_abort(
        paste0(
          "In biomarker ",
          biomarker,
          " the number of rule types (",
          n_rules,
          ") does not match the number of rule values (",
          length(rule_values),
          ")."
        )
      )
    }
    # grab biomarker data
    n_trues <- 0
    for (k in seq_len(n_rules)) {
      # evaluate rule k
      rule_type <- rule_types[[k]]
      rule_value <- rule_values[[k]]
      # in case there are rule_values that includes
      # two or more scores (rule_value) separated by ","
      if (!is.na(rule_value)) {
        rule_value <- stringr::str_trim(stringr::str_split_1(rule_value, ","))
      }
      n_matches <- sum(scores %in% rule_value, na.rm = TRUE)

      # determine how many matches are required
      # to consider that the rule is satisfied (n_required)
      if (rule_type == "any") {
        n_required <- 1 # at least one match
      } else if (rule_type == "all") {
        n_required <- length(scores) # all must match
      } else if (stringr::str_detect(rule_type, "^\\d+$")) {
        # e.g. 2, 3, 4 etc
        n_required <- as.numeric(rule_type) # at least n matches
        stopifnot(!is.na(n_required))
        stopifnot(n_required >= 1)
      } else if (rule_type == "else") {
        n_required <- 0 # always true
      } else {
        cli::cli_abort(paste0("Unknown rule type: ", rule_type))
      }

      # check if rule is satisfied
      if (n_matches >= n_required) {
        n_trues <- n_trues + 1
      }
    }

    if (n_trues == n_rules) {
      # all rules in a row of the biomarkers rules
      # spreadsheet are satisfied
      return(consolidated_value)
    }
    # otherwise, go to next row of rules
  }

  # if no rule matched throw an error
  cli::cli_abort(
    paste0(
      "No consolidation rule matched for scores: ",
      paste0(scores, collapse = ", "),
      ". Please check the consolidation rules for biomarker ",
      rules_df$biomarker[1]
    )
  )
}

#' @title Consolidate biomarkers from a translated TMA spreadsheet.
#' @description This function consolidates biomarker scores for each case.
#' @param biomarkers_file Path to the Excel file containing biomarker data (ie, output from `translate_scores()`).
#' @param output_file Optional path to the output file. If NULL, the function will not save the output.
#' @param biomarkers_data Optinally, pass a `data.frame` or `tibble` with biomarker data
#' instead of passing `biomarkers_file`. Used during re-consolidation in `tmatools()`
#' (usually not needed by end users).
#' @param late_na_ok If TRUE, NA values do not trigger error. Used during re-consolidation in `tmatools()`
#' (usually not needed by end users). Defaults to FALSE, which triggers an error if any NA is present in the scores to be consolidated.
#' @return A data frame with consolidated biomarker scores.
#' @inheritParams tmatools
#' @details 
#' The consolidation of individual scores will be placed in new columns with the same name as the biomarker but without the ".c1", ".c2" etc suffixes.
#' For instance, if the biomarker is "ER", the consolidated score will be placed
#' in a new column named "er" (lowercase). The original columns with the ".c1", ".c2" etc suffixes will be retained in the output.
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
#'  output_file = combined_tma_file,
#'  biomarker_sheet_index = 2,
#'  valid_biomarkers = c("ER", "p53") # optional, but recommended to avoid misspelling errors
#' )
#'
#' # deconvolute combined TMA dataset
#' deconvolute(
#'    tma_file = combined_tma_file,
#'    output_file = deconvoluted_tma_file
#' )
#' 
#' # grab biomarker rules file
#' biomarker_rules_file <- system.file("extdata", "biomarker_rules_example.xlsx",  package = "TMAtools")
#'
#' # translate numerical scores to nominal scores
#' translate_scores(
#'    biomarkers_file = deconvoluted_tma_file,
#'    biomarker_rules_file = biomarker_rules_file,
#'    output_file = translated_tma_file
#' )
#'
#' # and consolidate nominal scores for each case
#' consolidated_data <- consolidate_scores(
#'   biomarkers_file = translated_tma_file,
#'   output_file = consolidated_tma_file,
#'   biomarker_rules_file = biomarker_rules_file
#' )
#' print(consolidated_data)
consolidate_scores <- function(
  biomarkers_file = NULL,
  biomarker_rules_file = NULL,
  output_file = NULL,
  biomarkers_data = NULL,
  late_na_ok = FALSE
) {
  consolidation_df <- get_consolidation_rules_df(
    biomarker_rules_file = biomarker_rules_file
  )
  required_biomarkers <- unique(consolidation_df$biomarker)
  required_biomarkers <- required_biomarkers[required_biomarkers != "all"]

  ## read biomarker data for a TMA (deconvoluted numerical scores)
  if (!is.null(biomarkers_file)) {
    biomarkers_data <- readxl::read_excel(
      biomarkers_file,
      col_types = "text"
    )
  } else {
    if (is.null(biomarkers_data)) {
      cli::cli_abort("Must pass 'biomarkers_file or 'biomarkers_data'.")
    }
  }

  stopifnot(inherits(biomarkers_data, "data.frame"))

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
      biomarkers_data[[paste0(biomarker_name, ".c0")]] <- "Unk"
    }
  }

  # loop through biomarkers
  for (biomarker_name in required_biomarkers) {
    # rules for current biomarker
    rules_df <- consolidation_df[
      consolidation_df$biomarker == biomarker_name,
      ,
      drop = FALSE
    ]
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
          scores = unname(scores),
          late_na_ok = late_na_ok
        )
      }
    )
  }

  ## SAFETY CHECK for consolidation ----
  # check for any NA's in nominal scores
  # if any NA it is probably some numerical score that is not covered in the dictionary,
  # unless you are re-consolidating, in which case NAs are expected (eg,
  # if some Accession ID was repeated across TMAs, but not with the same number
  # of cores (should set late_na_ok=TRUE in that case).

  if (any(!complete.cases(biomarkers_data)) && !late_na_ok) {
    biomarkers_data_with_na <- biomarkers_data[
      !complete.cases(biomarkers_data),
    ]
    print(biomarkers_data_with_na)
    cli::cli_abort(
      "The cases above contain some NA after consolidation."
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
#' @keywords internal
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
  # except for "all" and quantitative cases
  biomarkers <- unique(consolidation_df$biomarker)
  for (biomarker in biomarkers) {
    if (biomarker != "all") {
      biomarker_sub_df <- consolidation_df[
        consolidation_df$biomarker == biomarker,
      ]
      no_else <- all(biomarker_sub_df$rule_type != "else")
      no_quant <- !any(biomarker_sub_df$rule_type %in% c("mean"))
      if (no_else && no_quant) {
        cli::cli_abort(
          paste0(
            "Biomarker ",
            biomarker,
            " is not quantitative and does not have an 'else' rule. ",
            "Please check consolidation rules."
          )
        )
      }
    }
  }

  return(consolidation_df)
}

#' @title Assess consolidated score per patient
#' @param rules_df data.frame with consolidation rules for a single biomarker
#' @param scores character vector with all scores for a given biomarker/patient
#' @param unknow_values character vector with values treated as unknown. Defaults to `c("Unk", "x")`.
#' @param late_na_ok If TRUE, NAs do not trigger error. Used for re-consolidation.
#' @inheritParams consolidate_scores
#' @keywords internal
#' @return consolidated value (as character).
consolidate_single_patient <- function(
  rules_df,
  scores,
  unknown_values = c("Unk", "x"),
  late_na_ok = FALSE
) {
  if (any(is.na(scores))) {
    if (!late_na_ok) {
      cli::cli_abort("Scores contain NA values.")
    } else {
      if (all(is.na(scores))) {
        return("Unk")
      }
      scores <- scores[!is.na(scores)]
    }
  }

  if (all(scores %in% unknown_values)) {
    return("Unk")
  }

  stopifnot(nrow(rules_df) > 0)

  if (nrow(rules_df) > 1) {
    if (any(rules_df$rule_type == "mean")) {
      msg <- paste0(
        "Please check consolidation rules as you cannot mix ",
        "qualitative scores with mean calculation over quantitative scores."
      )
      cli::cli_abort(msg)
    }
  } else {
    if (rules_df$rule_type == "mean") {
      valid_scores <- scores[!(scores %in% unknown_values)]
      return(as.character(mean(as.numeric(valid_scores))))
    }
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

#' @title Consolidate biomarkers from a translated TMA spreadsheet.
#' @description This function consolidates biomarker scores for each case.
#' @param biomarkers_file Path to the Excel file containing biomarker data (ie, output from `translate_scores()`).
#' @param output_file Optional path to the output file. If NULL, the function will not save the output.
#' @param biomarkers_data Optionally, pass a `data.frame` or `tibble` with biomarker data
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
#' tma_dir <- system.file("extdata", "tma1", package = "TMAtools")
#' combined_tma_file <- "combined_tma.xlsx"
#' deconvoluted_tma_file <- "deconvoluted_tma.xlsx"
#' translated_tma_file <- "translated_tma.xlsx"
#' consolidated_tma_file <- "consolidated_tma.xlsx"
#'
#' # combine TMA datasets
#' combine_tma_spreadsheets(
#'   tma_dir = tma_dir,
#'   output_file = combined_tma_file,
#'   biomarker_sheet_index = 2,
#'   valid_biomarkers = c("ER", "p53")
#' )
#'
#' # deconvolute combined TMA dataset
#' deconvolute(
#'   tma_file = combined_tma_file,
#'   output_file = deconvoluted_tma_file
#' )
#'
#' # grab biomarker rules file
#' biomarker_rules_file <- system.file(
#'   "extdata", "biomarker_rules_example.xlsx",
#'   package = "TMAtools"
#' )
#'
#' # translate numerical scores to nominal scores
#' translate_scores(
#'   biomarkers_file = deconvoluted_tma_file,
#'   biomarker_rules_file = biomarker_rules_file,
#'   output_file = translated_tma_file
#' )
#'
#' # consolidate nominal scores for each case
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

  for (biomarker_name in required_biomarkers) {
    has_col <- any(stringr::str_detect(
      colnames(biomarkers_data),
      stringr::fixed(biomarker_name, ignore_case = TRUE)
    ))
    if (!has_col) {
      biomarkers_data[[paste0(biomarker_name, ".c0")]] <- "Unk"
    }
  }

  parsed_rules <- list()
  for (biomarker_name in required_biomarkers) {
    rules_df <- consolidation_df[
      consolidation_df$biomarker == biomarker_name,
      ,
      drop = FALSE
    ]
    bm_rules <- list()
    for (i in seq_len(nrow(rules_df))) {
      rt <- rules_df$rule_type[i]
      rv <- rules_df$rule_value[i]
      if (is.na(rt) || rt == "") next
      types <- stringr::str_split_1(rt, ";")
      if (length(types) == 1 && types == "else") {
        bm_rules[[i]] <- list(type = "else", result = rules_df$consolidated_value[i])
        next
      }
      if (length(types) == 1 && types == "mean") {
        bm_rules[[i]] <- list(type = "mean")
        next
      }
      if (is.na(rv)) next
      values <- stringr::str_split_1(rv, ";")
      values <- lapply(values, function(x) stringr::str_trim(stringr::str_split_1(x, ",")))
      required <- integer(length(types))
      for (k in seq_along(types)) {
        tk <- types[k]
        if (tk == "any") {
          required[k] <- -1L
        } else if (tk == "all") {
          required[k] <- -2L
        } else if (stringr::str_detect(tk, "^\\d+$")) {
          required[k] <- as.integer(tk)
        } else {
          required[k] <- NA_integer_
        }
      }
      bm_rules[[i]] <- list(type = types, value = values, required = required, result = rules_df$consolidated_value[i])
    }
    if (length(bm_rules) > 1) {
      for (r in bm_rules) {
        if (is.character(r$type) && length(r$type) == 1 && r$type == "mean") {
          cli::cli_abort("Please check consolidation rules as you cannot mix qualitative scores with mean calculation over quantitative scores.")
        }
      }
    }
    parsed_rules[[biomarker_name]] <- bm_rules
  }

  consolidate_patient <- function(scores, rules, biomarker_name, late_na_ok) {
    if (any(is.na(scores))) {
      if (!late_na_ok) {
        cli::cli_abort("Scores contain NA values.")
      } else {
        if (all(is.na(scores))) return("Unk")
        scores <- scores[!is.na(scores)]
      }
    }
    if (length(scores) == 0 || all(scores %in% c("Unk", "x"))) return("Unk")

    for (r in rules) {
      if (is.character(r$type) && length(r$type) == 1 && r$type == "else") return(r$result)
      if (is.character(r$type) && length(r$type) == 1 && r$type == "mean") {
        valid <- scores[!(scores %in% c("Unk", "x"))]
        if (length(valid) == 0) return("Unk")
        return(as.character(mean(as.numeric(valid))))
      }
      n_trues <- 0L
      n_rules <- length(r$type)
      for (k in seq_len(n_rules)) {
        n_required <- r$required[k]
        if (is.na(n_required)) next
        if (n_required == -1L) {
          n_required <- 1L
        } else if (n_required == -2L) {
          n_required <- length(scores)
        }
        n_matches <- sum(scores %in% r$value[[k]], na.rm = TRUE)
        if (n_matches >= n_required) n_trues <- n_trues + 1L
      }
      if (n_trues == n_rules) return(r$result)
    }
    cli::cli_abort(paste0(
      "No consolidation rule matched for scores: ",
      paste0(scores, collapse = ", "),
      ". Please check the consolidation rules for biomarker ",
      biomarker_name
    ))
  }

  for (biomarker_name in required_biomarkers) {
    biomarker_scores <- dplyr::select(
      biomarkers_data,
      dplyr::starts_with(
        paste0(biomarker_name, ".c"),
        ignore.case = TRUE
      )
    )
    if (ncol(biomarker_scores) == 0L) {
      biomarkers_data[[tolower(biomarker_name)]] <- "Unk"
      next
    }
    bname_lower <- tolower(biomarker_name)
    biomarkers_data[[bname_lower]] <- apply(
      as.matrix(biomarker_scores),
      1L,
      function(s) consolidate_patient(unname(s), parsed_rules[[biomarker_name]], biomarker_name, late_na_ok)
    )
  }

  if (any(!complete.cases(biomarkers_data)) && !late_na_ok) {
    biomarkers_data_with_na <- biomarkers_data[!complete.cases(biomarkers_data), ]
    print(biomarkers_data_with_na)
    cli::cli_abort("The cases above contain some NA after consolidation.")
  }

  biomarkers_data <- biomarkers_data |>
    dplyr::select(
      -dplyr::contains(required_biomarkers),
      dplyr::contains(required_biomarkers)
    )

  if (!is.null(output_file)) {
    writexl::write_xlsx(biomarkers_data, output_file)
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
    cli::cli_abort(paste0("Biomarker rules file ", biomarker_rules_file, " does not exist."))
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
    cli::cli_abort(paste0(
      "Biomarker rules file ", biomarker_rules_file,
      " is missing columns required for consolidation: ",
      paste0(missing_cols, collapse = ", ")
    ))
  }
  biomarkers <- unique(consolidation_df$biomarker)
  for (biomarker in biomarkers) {
    if (biomarker != "all") {
      biomarker_sub_df <- consolidation_df[consolidation_df$biomarker == biomarker, ]
      no_else <- all(biomarker_sub_df$rule_type != "else", na.rm = TRUE)
      no_quant <- !any(biomarker_sub_df$rule_type %in% c("mean"))
      if (no_else && no_quant) {
        cli::cli_abort(paste0(
          "Biomarker ", biomarker,
          " is not quantitative and does not have an 'else' rule. ",
          "Please check consolidation rules."
        ))
      }
    }
  }
  return(consolidation_df)
}

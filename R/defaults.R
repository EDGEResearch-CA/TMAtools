#' @title Get default biomarkers
#' @description This function returns a vector of default biomarkers commonly used in TMA analysis.
#' @return A character vector of default biomarker names.
#' @export
#' @examples
#' library(TMAtools)
#' default_biomarkers <- get_default_biomarkers()
#' print(default_biomarkers)
get_default_biomarkers <- function() {
    c(
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

#' @title Get biomarker dictionary (default function)
#' @description This function returns a dictionary (named character vector) for
#' translating numerical scores to nominal scores for default biomarkers.
#' @param biomarker The name of the biomarker.
#' @return A named character vector representing the dictionary for the specified biomarker.
#' @export
#' @examples
#' library(TMAtools)
#' biomarkers <- c("ER", "TP53", "CTNNB1", "WT1")
#' for (biomarker in biomarkers) {
#'    print(
#'       c(biomarker, get_dict_default(biomarker)), 
#'       width = 10000  # for prettier printing
#'    )
#' }
get_dict_default <- function(biomarker) {
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

get_scores_translator_default <- function() {
  return(NA)
}
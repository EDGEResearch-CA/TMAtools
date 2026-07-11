#' Get template biomarker rules file
#' @param output_file Path to output file. The template will be copied to this path.
#' @param overwrite If TRUE and `output_file` exists, this function will
#' overwrite the contents of `output_file`. Defaults to FALSE.
#' @keywords internal 
get_biomarker_rules_template <- function(
  output_file = "biomarker_rules_template.xlsx",
  overwrite = FALSE
) {
  template_path <- system.file(
    "extdata",
    "biomarker_rules_enoc.xlsx",
    package = "TMAtools"
  )
  copied <- file.copy(template_path, output_file, overwrite = overwrite)
  if (!copied) {
    cli::cli_abort(paste0(
      "Could not copy template biomarker rules file to ",
      output_file,
      ". Check destination path and write permissions."
    ))
  }
  invisible(copied)
}

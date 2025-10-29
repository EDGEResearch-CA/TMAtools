#' Get template biomarker rules file
#' @param output_file Path to output file. The template will be copied to this path.
#' @param overwrite If TRUE and `output_file` exists, this function will
#' overwrite the contents of `output_file`. Defaults to FALSE.
#' @export
get_biomarker_rules_template <- function(
  output_file = "biomarker_rules_template.xlsx",
  overwrite = FALSE
) {
  template_path <- system.file(
    "extdata",
    "biomarker_rules_enoc.xlsx",
    package = "TMAtools"
  )
  invisible(file.copy(template_path, output_file, overwrite = overwrite))
}

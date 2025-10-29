#' Get template biomarker rules file
#' @param output_file Path to output file. The template will be copied to this path.
get_biomarker_rules_template <- function(output_file = "biomarker_rules_enoc.xlsx") {
    template_path <- system.file(
        "extdata", "biomarker_rules_enoc.xlsx",
        package = "TMAtools"
    )
    file.copy(template_path, output_file)
}

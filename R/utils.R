#' Internal utility helpers.
#'
#' These functions are package-private helpers used by rule parsing and preprocessing
#' paths. They are intentionally internal and not exported.
#'
#' @keywords internal
#' @noRd
.sanitize_rules_text <- function(x) {
  x_chr <- as.character(x)
  cleaned <- gsub("_x000d_", "", x_chr, fixed = TRUE)
  cleaned <- gsub("[[:cntrl:]]+", "", cleaned)
  cleaned <- gsub("[[:space:]]+", " ", cleaned)
  trimws(cleaned)
}

#' @keywords internal
#' @noRd
.sanitize_rules_df <- function(df, file_path, columns) {
  if (length(columns) == 0L) {
    return(df)
  }
  out <- df
  changed_count <- 0L

  for (column in columns) {
    original <- as.character(out[[column]])
    cleaned <- .sanitize_rules_text(out[[column]])
    changed_count <- changed_count + sum(
      (is.na(original) & !is.na(cleaned)) |
        (!is.na(original) & is.na(cleaned)) |
        (!is.na(original) & !is.na(cleaned) & (original != cleaned))
    )
    out[[column]] <- cleaned
  }

  if (changed_count > 0L) {
    cli::cli_inform(
      "Sanitized {changed_count} rule value artifact(s) while reading {.file {file_path}}."
    )
  }
  out
}

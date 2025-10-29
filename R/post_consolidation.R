assess_mmr_status <- function(biomarkers_data) {
  mmr_biomarkers <- c("mlh1", "msh2", "msh6", "pms2") # lowercase since using post-consolidation columns
  missing_biomarkers <- setdiff(mmr_biomarkers, colnames(biomarkers_data))
  if (length(missing_biomarkers) > 0) {
    cli::cli_abort(
      paste0(
        "Cannot assess MMR status because the following required biomarkers are missing: ",
        paste0(missing_biomarkers, collapse = ", ")
      )
    )
  }
  biomarkers_data[["mmr_ihc_4"]] <- apply(
    biomarkers_data,
    1,
    function(single_row) {
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
      } else if (msh6 == "Unk" || pms2 == "Unk") {
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
  return(biomarkers_data)
}
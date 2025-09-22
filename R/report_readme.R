# R/report_readme.R
#' Create a READ_ME worksheet
#' @param wb openxlsx workbook
#' @param study_name character
#' @param all_check data.frame from spec sheet "ALL_DATASETS"
#' @param simple_check data.frame from spec sheet "SINGLE_DATASETS"
#' @return wb
#' @keywords internal
create_readme_sheet <- function(wb, study_name = "", all_check, simple_check) {
  if ("READ_ME" %in% openxlsx::sheets(wb)) openxlsx::removeWorksheet(wb, "READ_ME")
  openxlsx::addWorksheet(wb, "READ_ME", tabColour = "orange")

  last_run <- format(Sys.time(), "%b %d, %Y at %I:%M %p %Z")
  intro_text <- c(
    paste("Raw Data Issue Tracker for", study_name),
    "",
    paste("Last run:", last_run),
    "",
    "This Excel file summarizes raw data issues identified from raw datasets.",
    "It includes non-standard issues, common issues, and dataset-specific issues.",
    "",
    "The table below describes non-standard issues in key tabs (highlighted in yellow):",
    ""
  )
  openxlsx::writeData(wb, sheet = "READ_ME", x = intro_text, startCol = 1, startRow = 1)

  # table：exclusive list for non-ALL_DATASETS
  table_data <- all_check |>
    dplyr::filter(OUTPUT_TAB != "ALL_DATASETS") |>
    dplyr::distinct(OUTPUT_TAB, CHECKPOINT) |>
    dplyr::rename(Tab_name = OUTPUT_TAB, Checkpoint = CHECKPOINT)
  table_start_row <- length(intro_text) + 1
  openxlsx::writeData(wb, "READ_ME", table_data, startCol = 1, startRow = table_start_row, borders = "columns")

  common_issues <- all_check |>
    dplyr::filter(OUTPUT_TAB == "ALL_DATASETS") |>
    dplyr::distinct(CHECKPOINT) |>
    dplyr::pull(CHECKPOINT)

  dataset_specific <- simple_check |>
    dplyr::distinct(DATASET, CHECKPOINT) |>
    dplyr::mutate(text = paste0("-- For ", DATASET, ": ", CHECKPOINT)) |>
    dplyr::pull(text)

  description_text <- c(
    "",
    "Other issues are organized in dataset-specific tabs and include the following types:",
    "",
    "1. **Common issues**, include:",
    paste0("-- ", common_issues),
    "",
    "2. Dataset-specific issues, include:",
    dataset_specific,
    "",
    "Notes:",
    "-- This sheet can be updated automatically by the program and manually by reviewers.",
    "-- Issue type is recorded in 'Issue_type' (use 'Manual' for manually added issues)."
  )
  description_start_row <- table_start_row + nrow(table_data) + 2
  openxlsx::writeData(wb, "READ_ME", description_text, startCol = 1, startRow = description_start_row)

  wb
}

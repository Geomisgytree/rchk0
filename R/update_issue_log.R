#' Update issue log by merging new and old Excel sheets
#'
#' This function updates the issue log workbook by comparing new issues
#' detected in the current run with historical issues recorded in
#' a master Excel file.
#'
#' @param wb An openxlsx Workbook object containing the newly generated issue sheets
#' @param master_path Path to the master Excel file (historical issue log)
#'
#' @return The updated workbook object
#' @export
#'
#' @importFrom dplyr mutate select filter arrange bind_rows full_join row_number
#' @importFrom tidyr fill
#' @importFrom stringr str_detect
#' @importFrom openxlsx addWorksheet writeData readWorkbook
#' @importFrom readxl excel_sheets read_excel
#' @importFrom glue glue
update_issue_log <- function(wb, master_path) {
  ## sheet_keys: define custom join keys per dataset
  sheet_keys <- list(
    # "ex1001" = c("SUBJID", "COUNTRY", "SITENUM", "EGROUP", "EVENT", "EVENTDT", "EXOCCUR"),
    # "cm1001" = c("SUBJID", "COUNTRY", "SITENUM", "EVENT", "CMTRT", "CMSTDAT", "PREFLABEL"),
    # "ds6001" = c("SUBJID", "COUNTRY", "SITENUM", "EVENT")
  )

  if (file.exists(master_path)) {
    sheets <- names(wb)
    sheets <- sheets[!sheets %in% c("READ_ME")]

    for (sheet in sheets) {
      new <- openxlsx::readWorkbook(wb, sheet = sheet, detectDates = FALSE) |>
        dplyr::select(-PPD_Comment_or_resolution) |>
        dplyr::mutate(dplyr::across(everything(), ~na_if(.x, "")))

      if (!(sheet %in% readxl::excel_sheets(master_path))) next

      fill_cols <- c("Issue_noted_by_Lilly_Stats", "Status", "Issue_type", "PPD_Comment_or_resolution")
      old <- readxl::read_excel(master_path, sheet = sheet, col_types = "text") |>
        dplyr::mutate(row_num = dplyr::row_number()) |>
        tidyr::fill(dplyr::all_of(fill_cols), .direction = "down")

      required_cols <- c("SUBJECT_ID", "Issue_noted_by_Lilly_Stats", "Issue_type", "Status")
      if (!all(required_cols %in% colnames(new))) next

      old_manual <- dplyr::filter(old, Issue_type == "Manual")
      old_auto   <- dplyr::filter(old, Issue_type == "Automatic" | is.na(Issue_type))
      new_auto   <- new

      if (!is.null(sheet_keys[[sheet]])) {
        candidate_keys <- sheet_keys[[sheet]]
        common_keys <- intersect(candidate_keys, intersect(names(old_auto), names(new_auto)))
        if (length(common_keys) == 0) {
          warning(paste0("No valid join keys for sheet: ", sheet))
          next
        }
        by_cols <- common_keys
      } else {
        common_cols <- intersect(names(old_auto), names(new_auto))
        by_cols <- setdiff(common_cols, c("Status", "Issue_noted_by_Lilly_Stats", "Issue_type"))
      }

      merged_auto <- dplyr::full_join(old_auto, new_auto, by = by_cols, suffix = c("_old", "_new"))
      today_tag <- format(Sys.Date(), "%d%b%Y") |> toupper()

      updated_auto <- merged_auto |>
        dplyr::mutate(
          Status = dplyr::case_when(
            stringr::str_detect(tolower(Status_old), "permanent") ~ Status_old,
            is.na(Status_old) & is.na(Status_new) ~ "Closed",
            is.na(Status_old) & !is.na(Status_new) ~ "New",
            !is.na(Status_old) & !is.na(Status_new) & Status_old == "Closed" ~ "Recurred",
            !is.na(Status_old) & !is.na(Status_new) ~ "Open",
            !is.na(Status_old) & is.na(Status_new) ~ "Closed",
            TRUE ~ Status_old
          ),
          Issue_type = dplyr::coalesce(Issue_type_new, Issue_type_old),
          Issue_noted_by_Lilly_Stats = dplyr::case_when(
            is.na(Status_new) | stringr::str_detect(tolower(Status_old), "permanent") ~ Issue_noted_by_Lilly_Stats_old,
            is.na(Issue_noted_by_Lilly_Stats_new) ~ Issue_noted_by_Lilly_Stats_old,
            is.na(Issue_noted_by_Lilly_Stats_old) ~ paste0(today_tag, ": ", Issue_noted_by_Lilly_Stats_new),
            TRUE ~ paste(
              Issue_noted_by_Lilly_Stats_old,
              paste0(today_tag, ": ", Issue_noted_by_Lilly_Stats_new),
              sep = "\n"
            )
          )
        ) |>
        dplyr::select(-Status_new, -Status_old,
                      -Issue_noted_by_Lilly_Stats_old, -Issue_noted_by_Lilly_Stats_new,
                      -Issue_type_new, -Issue_type_old)

      trailing_cols <- c("Issue_type", "Issue_noted_by_Lilly_Stats", "PPD_Comment_or_resolution", "Status")
      other_cols <- setdiff(names(updated_auto), trailing_cols)

      updated_auto <- updated_auto |>
        dplyr::select(dplyr::all_of(c(other_cols, trailing_cols))) |>
        dplyr::select(-dplyr::matches("_old$"))
      names(updated_auto) <- gsub("_new$", "", names(updated_auto))

      updated_sheet <- dplyr::bind_rows(updated_auto, old_manual) |>
        dplyr::arrange(row_num) |>
        dplyr::select(-row_num)

      openxlsx::writeData(wb, sheet, updated_sheet)
    }

    ## Add missing sheets from old master
    old_sheets <- setdiff(readxl::excel_sheets(master_path), c("READ_ME"))
    missing_sheets <- setdiff(old_sheets, names(wb))
    for (sheet in missing_sheets) {
      old_data <- readxl::read_excel(master_path, sheet = sheet, col_types = "text")
      openxlsx::addWorksheet(wb, sheet)
      openxlsx::writeData(wb, sheet = sheet, old_data)
    }

  } else {
    warning("Master log not found: ", master_path, ". Returning empty workbook.")
  }

  return(wb)
}

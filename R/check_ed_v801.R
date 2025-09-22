#' Check consistency between ED and V801 visits
#'
#' This function performs two checks:
#' 1. For subjects having both ED and V801, ED date is on or after V801.
#' 2. Regulatory visits (V1–V10) occurring after ED across all datasets (except diary data).
#'
#' @param datasets_pool A named list of datasets (data frames).
#' @param wb An openxlsx workbook object to which results will be written.
#' @param dataset_no A character vector of dataset names to exclude (e.g., diary data).
#' @param visit_info_df A data frame describing dataset, subject, site, visit/date column info.
#' @param output_tab The name of the worksheet to create in the Excel output.
#'
#' @return The workbook `wb` with a new sheet added if issues are found.
#' @export
#'
#' @import dplyr stringr tidyr openxlsx
check_ed_v801 <- function(datasets_pool, wb,
                          dataset_no = NULL,
                          visit_info_df = NULL,
                          output_tab = NULL) {

  non_diary <- setdiff(names(datasets_pool), dataset_no) ## exclude un-used datasets

  if (length(non_diary) == 0) {
    warning("ED and V801 check skipped: no non-diary datasets")
    return(invisible(NULL))
  }

  ## Collect all the visits for ED/V801/REG
  ed_v801_list <- list()
  for (ds in non_diary) {
    df <- datasets_pool[[ds]]
    info_row <- visit_info_df %>% dplyr::filter(dataset == ds)

    if (nrow(info_row) == 0) {
      warning("Dataset ", ds, " not found in visit_info_df. Skipped.")
      next
    }

    date_col  <- info_row$visit_date_col
    visit_col <- info_row$visit_label_col

    if (is.na(date_col) || is.na(visit_col)) {
      warning("Dataset ", ds, " missing date/visit column info. Skipped.")
      next
    }

    tmp <- df %>%
      dplyr::transmute(
        dataset    = ds,
        SUBJECT_ID = as.character(SUBJECT_ID),
        VISIT      = .data[[visit_col]],
        VISIT_DATE = as.Date(.data[[date_col]])
      ) %>%
      dplyr::filter(!is.na(VISIT_DATE)) %>%
      dplyr::mutate(
        VISIT_TYPE = dplyr::case_when(
          # Visit 801
          stringr::str_detect(VISIT, regex("\\b(?:EVV?\\s*801|V\\s*801|VISIT\\s*801|801)\\b", ignore_case = TRUE)) ~ "V801",
          # ED
          stringr::str_detect(VISIT, regex("^ED$|EARLY DISCONTINUATION", ignore_case = TRUE)) ~ "ED",
          # Regulatory visits V1–V10
          stringr::str_detect(VISIT, regex("\\b(?:EVV?\\s*([1-9]|10)|V\\s*([1-9]|10)|VISIT\\s*0*([1-9]|10)|0*([1-9]|10))\\b", ignore_case = TRUE)) ~ "REG",
          TRUE ~ "OTHER"
        )
      ) %>%
      dplyr::distinct(dataset, SUBJECT_ID, VISIT, VISIT_DATE, VISIT_TYPE)

    ed_v801_list[[ds]] <- tmp
  }

  visit_all <- dplyr::bind_rows(ed_v801_list)

  if (nrow(visit_all) == 0) {
    warning("ED vs V801 check skipped: visit data not found.")
    return(invisible(NULL))
  }

  ## Only proceed if either ED or V801 exists
  if (!any(visit_all$VISIT_TYPE %in% c("ED","V801"))) {
    warning("Skipped ED vs. V801 check: no ED or V801 visits.")
    return(invisible(NULL))
  }

  # Issue 1: ED >= V801
  date_lookup <- visit_all %>%
    dplyr::filter(VISIT_TYPE %in% c("ED","V801")) %>%
    dplyr::group_by(SUBJECT_ID, VISIT_TYPE) %>%
    dplyr::summarise(min_date = min(VISIT_DATE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = VISIT_TYPE, values_from = min_date)

  issue1 <- date_lookup %>%
    dplyr::filter(!is.na(ED) & !is.na(V801) & ED >= V801) %>%
    dplyr::mutate(
      Issue_type = "Automatic",
      Issue_noted_by_Lilly_Stats = "For subjects having both ED and V801, ED date is on or after V801",
      PPD_Comment_or_resolution = "",
      Status = "New"
    )

  # Issue 2: Regulatory visits after ED
  reg_after_ed <- visit_all %>%
    dplyr::filter(VISIT_TYPE == "REG") %>%
    dplyr::left_join(date_lookup %>% dplyr::select(SUBJECT_ID, ED), by = "SUBJECT_ID") %>%
    dplyr::filter(!is.na(ED), VISIT_DATE > ED) %>%
    dplyr::mutate(
      Issue_type = "Automatic",
      Issue_noted_by_Lilly_Stats = "Regulatory visit after ED",
      PPD_Comment_or_resolution = "",
      Status = "New"
    ) %>%
    dplyr::select(-ED)

  if (nrow(issue1) > 0 | nrow(reg_after_ed) > 0) {
    openxlsx::addWorksheet(wb, output_tab, tabColour = "#FFFF99")
  }

  if (nrow(issue1) > 0) {
    openxlsx::writeData(wb, sheet = output_tab, x = issue1, startRow = 1)
  }

  if (nrow(reg_after_ed) > 0) {
    start_row <- ifelse(nrow(issue1) > 0, nrow(issue1) + 3, 1)
    openxlsx::writeData(wb, sheet = output_tab, x = reg_after_ed, startRow = start_row)
  }

  invisible(NULL)
}

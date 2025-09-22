#' Check visit gaps between consecutive visits
#'
#' This function checks that consecutive scheduled visits are more than
#' \code{day_lob} days (default: 7 days / 1 week) and less than
#' \code{day_hib} days (default: 21 days / 3 weeks) apart.
#'
#' Visits such as 1, 801, 802, 803 are excluded from the check.
#'
#' @param datasets_pool Named list of raw datasets
#' @param wb An openxlsx Workbook object
#' @param visit_info_df Metadata with dataset, visit_label_col, visit_date_col
#' @param output_tab Name of Excel worksheet to create
#' @param day_lob Minimum day gap (default: 7)
#' @param day_hib Maximum day gap (default: 21)
#'
#' @return Writes a worksheet with detected issues into \code{wb}
#' @keywords internal
check_visit_gap <- function(datasets_pool, wb,
                            visit_info_df = NULL,
                            output_tab = NULL,
                            day_lob = 7,
                            day_hib = 21) {

  available_ds <- names(datasets_pool)
  visit_gap_issues <- list()

  for (ds_name in available_ds) {
    df <- datasets_pool[[ds_name]]
    info_row <- dplyr::filter(visit_info_df, dataset == ds_name)

    if (nrow(info_row) == 0) {
      warning("Dataset ", ds_name, " not found in visit_info_df. Skipped.")
      next
    }

    visit_col <- info_row$visit_label_col
    date_col  <- info_row$visit_date_col

    ## skip if no visit or date column
    if (is.na(visit_col) || is.na(date_col)) {
      warning("Dataset ", ds_name, " missing visit/date column info. Skipped.")
      next
    }

    ## filter visits
    tmp_df <- df |>
      dplyr::filter(!is.na(.data[[visit_col]])) |>
      dplyr::filter(
        grepl("(?i)^(evV)?\\d+$", .data[[visit_col]]) &
          !grepl("(?i)unscheduled", .data[[visit_col]])
      ) |>
      dplyr::transmute(
        SUBJECT_ID,
        VISIT      = .data[[visit_col]],
        VISIT_DATE = as.Date(.data[[date_col]])
      ) |>
      dplyr::arrange(SUBJECT_ID, VISIT_DATE)

    ## consecutive visits
    tmp_df <- tmp_df |>
      dplyr::mutate(VISIT_NUM = as.numeric(stringr::str_extract(VISIT, "\\d+"))) |>
      dplyr::filter(!is.na(VISIT_NUM)) |>
      dplyr::arrange(SUBJECT_ID, VISIT_NUM, VISIT_DATE) |>
      dplyr::group_by(SUBJECT_ID) |>
      dplyr::mutate(
        NEXT_VISIT_NUM  = dplyr::lead(VISIT_NUM),
        NEXT_VISIT_DATE = dplyr::lead(VISIT_DATE),
        DAYS_DIFF       = as.numeric(difftime(NEXT_VISIT_DATE, VISIT_DATE, units = "days")),
        WEEKS_DIFF      = round(DAYS_DIFF / 7, 1)
      ) |>
      dplyr::ungroup() |>
      dplyr::filter(
        !is.na(NEXT_VISIT_NUM),
        abs(NEXT_VISIT_NUM - VISIT_NUM) == 1,
        !VISIT_NUM %in% c(1, 801, 802, 803),
        DAYS_DIFF <= day_lob | DAYS_DIFF >= day_hib
      ) |>
      dplyr::mutate(
        issue = paste0("Visit ", VISIT_NUM, " and Visit ", NEXT_VISIT_NUM,
                       " have a ", WEEKS_DIFF, " wks (", DAYS_DIFF, " days) gap.")
      )

    if (nrow(tmp_df) > 0) {
      visit_gap_issues[[ds_name]] <- tmp_df |>
        dplyr::select(SUBJECT_ID, VISIT, VISIT_DATE,
                      NEXT_VISIT_NUM, NEXT_VISIT_DATE,
                      DAYS_DIFF, WEEKS_DIFF, issue)
    }
  }

  if (length(visit_gap_issues) > 0) {
    final_issues <- dplyr::bind_rows(visit_gap_issues, .id = "dataset_name") |>
      dplyr::mutate(
        Issue_type = "Automatic",
        Issue_noted_by_Lilly_Stats = paste0(
          "Consecutive visits should be >", day_lob/7,
          " wk(s) and <", day_hib/7, " wk(s)"
        ),
        PPD_Comment_or_resolution = "",
        Status = "New"
      )

    openxlsx::addWorksheet(wb, output_tab, tabColour = "#FFFF99")
    openxlsx::writeData(wb, sheet = output_tab, x = final_issues)
  }

  invisible(NULL)
}

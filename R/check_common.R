# R/check_common.R
# Common issue check functions for raw data tracker

#' Wrap summary and raw issue outputs
#'
#' @param df_summary A summary data.frame
#' @param df_raw A raw data.frame
#' @param idx Optional index
#' @param label Optional label
#' @return A list with `summary` and `raw`
#' @keywords internal
issue_envelope <- function(df_summary, df_raw, idx = NULL, label = NULL) {
  list(summary = df_summary, raw = df_raw)
}

#' Post-process raw issue records
#'
#' @param raw A data.frame of issues
#' @param issue A string with issue description
#' @return data.frame with added metadata columns
#' @keywords internal
raw_process <- function(raw, issue) {
  dplyr::mutate(raw,
                Issue_type = "Automatic",
                Issue_noted_by_Lilly_Stats = issue,
                PPD_Comment_or_resolution = "",
                Status = "New"
  )
}


#' Post-process summary issue records
#'
#' @param sum A summary data.frame
#' @param issue Character issue label
#' @param ds_name Dataset name
#' @param date_col Optional date column
#' @return A standardized summary data.frame
#' @keywords internal
sum_process <- function(sum, issue, ds_name, date_col = NULL) {
  sum |>
    dplyr::group_by(SITEID, SUBJECT_ID) |>
    dplyr::summarise(
      DATASET = ds_name,
      ISSUE = issue,
      FIRST_DETECTED_DATE = Sys.Date(),
      LAST_CHECKED_DATE   = Sys.Date(),
      STATUS = "New",
      .groups = "drop"
    ) |>
    dplyr::select(DATASET, SUBJECT_ID, SITEID, ISSUE,
                  FIRST_DETECTED_DATE, LAST_CHECKED_DATE, STATUS)
}

#' Check for missing key variables
#'
#' @param datasets_pool Named list of datasets
#' @param wb An openxlsx Workbook
#' @param target_vars Key variables to check
#' @param dataset_no Names of datasets to exclude
#' @param visit_info_df Visit info metadata
#' @param output_tab Optional output tab
#' @return Writes results into workbook, invisibly returns NULL
#' @export
check_missing_key_vars <- function(datasets_pool, wb,
                                   target_vars = NULL,
                                   dataset_no = NULL,
                                   visit_info_df = NULL,
                                   output_tab = NULL) {

  datasets <- setdiff(names(datasets_pool), dataset_no)
  if (length(datasets) == 0) {
    warning("Missing key var check skipped: no datasets used")
    return(invisible(NULL))
  }

  for (ds in datasets) {
    df <- datasets_pool[[ds]]
    common <- intersect(target_vars, names(df))
    if (!length(common)) {
      message("Dataset ", ds, " skipped: no key vars found.")
      next
    }

    char_vars <- intersect(common, names(df)[sapply(df, is.character)])
    df <- dplyr::mutate(df, dplyr::across(dplyr::all_of(char_vars), ~na_if(.x, "")))

    flagged <- dplyr::filter(df, if_any(dplyr::all_of(common), is.na))
    if (nrow(flagged) == 0) next

    flagged <- dplyr::mutate(flagged,
                             Issue_type = "Automatic",
                             Issue_noted_by_Lilly_Stats = "Missing values in VISIT/VISITNUM/EVENT/EVENTDEF/EVENTEID",
                             PPD_Comment_or_resolution = "",
                             Status = "New"
    )
    flagged[] <- lapply(flagged, as.character)

    if (!(ds %in% names(wb))) {
      openxlsx::addWorksheet(wb, ds)
      start_row <- 1; write_header <- TRUE
    } else {
      existing_df <- tryCatch(openxlsx::readWorkbook(wb, sheet = ds), error = function(e) NULL)
      if (is.null(existing_df)) {
        start_row <- 1; write_header <- TRUE
      } else {
        start_row <- nrow(existing_df) + 2; write_header <- FALSE
      }
    }
    openxlsx::writeData(wb, sheet = ds, x = flagged,
                        startRow = start_row, colNames = write_header)
  }
  invisible(NULL)
}

#' Check for duplicate visits with the same date
#'
#' @param datasets_pool Named list of datasets
#' @param wb Workbook
#' @param target_vars Optional
#' @param dataset_no Datasets to exclude
#' @param visit_info_df Visit info
#' @param output_tab Output tab
#' @param custom_code_list Named list of custom check functions
#' @export
check_duplicate_visit_date <- function(datasets_pool, wb,
                                       target_vars = NULL,
                                       dataset_no = NULL,
                                       visit_info_df = NULL,
                                       output_tab = NULL,
                                       custom_code_list = NULL) {

  datasets <- setdiff(names(datasets_pool), dataset_no)
  for (ds_name in datasets) {
    df <- datasets_pool[[ds_name]]
    if (is.null(df) || nrow(df) == 0) { message("Dataset empty: ", ds_name); next }

    info_row <- dplyr::filter(visit_info_df, dataset == ds_name)
    if (nrow(info_row) == 0) { message("Skip: ", ds_name, " no visit_info."); next }

    visit_date_col <- info_row$visit_date_col
    visit_label_col <- info_row$visit_label_col
    if (is.na(visit_date_col) || !(visit_date_col %in% names(df))) {
      warning("Skip: ", ds_name, " - visit_date_col not found."); next
    }

    df <- dplyr::filter(df, !is.na(.data[[visit_date_col]]))
    if (!is.na(visit_label_col) && visit_label_col %in% names(df)) {
      df <- dplyr::filter(df, !grepl("unscheduled", .data[[visit_label_col]], ignore.case = TRUE))
    }

    if (!is.null(custom_code_list) && ds_name %in% names(custom_code_list)) {
      dups <- custom_code_list[[ds_name]](df, visit_date_col)
    } else {
      dups <- df |>
        dplyr::group_by(SUBJECT_ID, .data[[visit_date_col]]) |>
        dplyr::filter(dplyr::n() > 1) |>
        dplyr::ungroup()
    }

    if (nrow(dups) == 0) next
    dups <- raw_process(dplyr::arrange(dups, SUBJECT_ID, .data[[visit_date_col]], .data[[visit_label_col]]),
                        issue = "Duplicate visits with the same date")

    if (!(ds_name %in% names(wb))) {
      openxlsx::addWorksheet(wb, ds_name); start_row <- 1; write_header <- TRUE
    } else {
      existing_df <- tryCatch(openxlsx::readWorkbook(wb, sheet = ds_name), error = function(e) NULL)
      if (is.null(existing_df)) { start_row <- 1; write_header <- TRUE }
      else { start_row <- nrow(existing_df) + 2; write_header <- FALSE }
    }
    openxlsx::writeData(wb, ds_name, dups, startRow = start_row, colNames = write_header)
  }
  invisible(NULL)
}

#' Check for visits after ED
#'
#' @param datasets_pool Named list of datasets
#' @param wb Workbook
#' @param dataset_no Excluded datasets
#' @param visit_info_df Visit info metadata
#' @param output_tab Output tab
#' @param other_datasets Character vector of datasets to check
#' @return A named list, each element is a data frame of records
#'   with visits after ED for the corresponding dataset.
#'   If no datasets have violations, returns an empty list.
#' @export
check_visit_after_ED <- function(datasets_pool, wb,
                                 dataset_no = NULL,
                                 visit_info_df = NULL,
                                 output_tab = NULL,
                                 other_datasets = NULL) {
  ed_ds <- datasets_pool[["ds6001"]]
  if (is.null(ed_ds) || !("DSSTDAT" %in% names(ed_ds))) {
    warning("ED dataset missing or DSSTDAT not found.")
    return(NULL)
  }

  ed_dates <- ed_ds |>
    dplyr::filter(FORMEID %in% c("DS6001_LV6", "DS6001_LV5"), DSSTDAT != "") |>
    dplyr::mutate(ED_DATE = as.Date(DSSTDAT)) |>
    dplyr::select(SUBJECT_ID, ED_DATE)

  # --- collect results here ---
  results_list <- list()

  for (ds_name in other_datasets) {
    df <- datasets_pool[[ds_name]]
    if (is.null(df) || nrow(df) == 0) {
      message("Empty: ", ds_name)
      next
    }

    visit_date_col <- dplyr::filter(visit_info_df, dataset == ds_name) |> dplyr::pull(visit_date_col)
    if (is.na(visit_date_col) || !(visit_date_col %in% names(df))) {
      warning("Skip: ", ds_name, " - visit_date_col not found.")
      next
    }

    df <- dplyr::mutate(df, VISIT_DATE = as.Date(.data[[visit_date_col]])) |>
      dplyr::inner_join(ed_dates, by = "SUBJECT_ID") |>
      dplyr::filter(!is.na(VISIT_DATE), !is.na(ED_DATE))

    filtered <- dplyr::filter(df, VISIT_DATE > ED_DATE)
    if ("VISITNUM" %in% names(filtered)) {
      filtered <- dplyr::filter(filtered, !(VISITNUM %in% c("999", "997", "801", "802", "803")))
    }

    if (nrow(filtered) == 0) next

    # --- save to results list ---
    results_list[[ds_name]] <- filtered

    # --- write to Excel as before ---
    raw <- raw_process(dplyr::select(filtered, -VISIT_DATE, -ED_DATE),
                       "Visit occurred after ED date")

    if (!(ds_name %in% names(wb))) {
      openxlsx::addWorksheet(wb, ds_name); start_row <- 1; write_header <- TRUE
    } else {
      existing_df <- tryCatch(openxlsx::readWorkbook(wb, sheet = ds_name), error = function(e) NULL)
      if (is.null(existing_df)) {
        start_row <- 1; write_header <- TRUE
      } else {
        start_row <- nrow(existing_df) + 2; write_header <- FALSE
      }
    }
    openxlsx::writeData(wb, ds_name, raw, startRow = start_row, colNames = write_header)
  }

  # --- return collected results ---
  return(results_list)
}

#' Check for overlapped visits
#'
#' @param datasets_pool Named list of datasets
#' @param wb Workbook
#' @param dataset_no Excluded datasets
#' @param visit_info_df Visit info metadata
#' @param output_tab Output tab
#'
#' @return A named list, each element is a data frame of records
#'   with overlapped visits for the corresponding dataset.
#'   If no datasets have overlaps, returns an empty list.
#'
#' @export
check_visit_overlap <- function(datasets_pool, wb,
                                dataset_no = NULL,
                                visit_info_df = NULL,
                                output_tab = NULL) {
  datasets <- setdiff(names(datasets_pool), dataset_no)

  # --- collect results here ---
  results_list <- list()

  for (ds_name in datasets) {
    ds <- datasets_pool[[ds_name]]
    if (is.null(ds) || nrow(ds) == 0) { message("Empty: ", ds_name); next }

    info_row <- dplyr::filter(visit_info_df, dataset == ds_name)
    if (nrow(info_row) == 0) { message("Skip: ", ds_name, " no visit_info."); next }

    visit_date_col <- info_row$visit_date_col
    visit_label_col <- info_row$visit_label_col
    if (is.na(visit_date_col) || !(visit_date_col %in% names(ds))) {
      warning("Skip: ", ds_name, " - visit_date_col not found."); next
    }

    if ("VISITNUM" %in% names(ds)) {
      ds <- dplyr::mutate(ds, VISIT_NUM = suppressWarnings(as.numeric(VISITNUM))) |>
        dplyr::filter(!is.na(VISIT_NUM), !(VISIT_NUM %in% c(999, 997)))
    } else if (!is.na(visit_label_col) && visit_label_col %in% names(ds)) {
      ds <- ds |>
        dplyr::filter(!is.na(.data[[visit_label_col]]),
                      !grepl("unscheduled", .data[[visit_label_col]], ignore.case = TRUE)) |>
        dplyr::mutate(
          VISIT_NUM = suppressWarnings(as.numeric(stringr::str_extract(.data[[visit_label_col]], "\\d+")))
        ) |>
        dplyr::filter(!is.na(VISIT_NUM))
    } else {
      warning("Skip: ", ds_name, " - cannot determine VISITNUM."); next
    }

    ds <- dplyr::mutate(ds, VISIT_DATE = as.Date(.data[[visit_date_col]])) |>
      dplyr::filter(!is.na(VISIT_DATE)) |>
      dplyr::group_by(SITEID, SUBJECT_ID) |>
      dplyr::arrange(VISIT_NUM, .by_group = TRUE) |>
      dplyr::mutate(PREV_DATE = dplyr::lag(VISIT_DATE),
                    OVERLAP_FL = VISIT_DATE < PREV_DATE) |>
      dplyr::ungroup()

    overlap_rows <- dplyr::filter(ds, OVERLAP_FL)
    prior_rows <- dplyr::inner_join(
      ds,
      dplyr::select(overlap_rows, SITEID, SUBJECT_ID, CUR_VISITNUM = VISIT_NUM, PREV_DATE),
      by = c("SITEID", "SUBJECT_ID", "VISIT_DATE" = "PREV_DATE")
    )

    raw0 <- dplyr::bind_rows(overlap_rows, prior_rows) |>
      dplyr::arrange(SITEID, SUBJECT_ID, VISIT_NUM, VISIT_DATE) |>
      dplyr::select(-PREV_DATE, -OVERLAP_FL, -VISIT_DATE, -CUR_VISITNUM, -VISIT_NUM)

    raw <- raw_process(raw0, "Overlapped visits: later visit has earlier date")
    if (nrow(raw) == 0) next

    # --- save to results list ---
    results_list[[ds_name]] <- raw

    # --- write to Excel as before ---
    if (!(ds_name %in% names(wb))) {
      openxlsx::addWorksheet(wb, ds_name); start_row <- 1; write_header <- TRUE
    } else {
      existing_df <- tryCatch(openxlsx::readWorkbook(wb, sheet = ds_name), error = function(e) NULL)
      if (is.null(existing_df)) { start_row <- 1; write_header <- TRUE }
      else { start_row <- nrow(existing_df) + 2; write_header <- FALSE }
    }
    openxlsx::writeData(wb, ds_name, raw, startRow = start_row, colNames = write_header)
  }

  # --- return collected results ---
  return(results_list)
}




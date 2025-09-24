#' Check ED visit consistency across datasets
#'
#' Patients having ED visit in DS6001, but no ED visits in lab or eCOA datasets.
#' Compares ED dates across ds6001 and other datasets, and reports missing or inconsistent values.
#'
#' @param datasets_pool Named list of raw datasets
#' @param wb An openxlsx Workbook object
#' @param other_datasets Character vector of datasets to check (e.g., c("lab", "ecoa"))
#' @param output_tab Name of Excel worksheet to create
#' @param visit_info_df Metadata with dataset, visit_label_col, visit_date_col
#'
#' @return Writes a worksheet with detected issues into \code{wb}
#' @export
check_ED <- function(datasets_pool,
                     wb,
                     other_datasets = NULL,
                     output_tab = NULL,
                     visit_info_df = NULL) {

  ds6001 <- datasets_pool[["ds6001"]]
  sv1001 <- datasets_pool[["sv1001"]]

  if (is.null(ds6001) | is.null(sv1001)) {
    warning("ds6001 or sv1001 missing. Skipped.")
    return(invisible(NULL))
  }

  ## Required variables
  req_vs <- c("DSSTDAT", "SUBJECT_ID")
  miss_vs <- setdiff(req_vs, names(ds6001))
  if (length(miss_vs) > 0) {
    warning("ED check skipped: ds6001 lacks variable(s) ",
            paste(miss_vs, collapse = ", "))
    return(invisible(NULL))
  }

  ## Extract ED date from ds6001
  ED_DS <- ds6001 |>
    dplyr::filter(FORMEID %in% c("DS6001_LV6", "DS6001_LV5"),
                  DSSTDAT != "") |>
    dplyr::mutate(DS_ED = as.Date(DSSTDAT)) |>
    dplyr::transmute(SUBJECT_ID, SITEID, DS_ED = as.character(DS_ED))

  available_others <- intersect(other_datasets, names(datasets_pool))
  if (length(available_others) == 0) {
    warning("No valid other datasets found in other_datasets. Skipped.")
    return(invisible(NULL))
  }

  ## Collect ED dates from other datasets
  ed_data_list <- list()
  for (ds_name in available_others) {
    df <- datasets_pool[[ds_name]]
    info_row <- dplyr::filter(visit_info_df, dataset == ds_name)
    if (nrow(info_row) == 0) {
      warning("Dataset ", ds_name, " not found in visit_info_df. Skipped."); next
    }
    visit_col <- info_row$visit_label_col
    date_col  <- info_row$visit_date_col
    if (is.na(visit_col) || is.na(date_col)) {
      warning("Dataset ", ds_name, " missing visit/date column info. Skipped."); next
    }
    if (!visit_col %in% names(df) || !date_col %in% names(df)) {
      message("Skipped ", ds_name, ": required cols not found."); next
    }

    ed_sub <- dplyr::filter(df,
                            stringr::str_detect(.data[[visit_col]], "(?i)\\bED\\b|EARLY DISCONTINUATION|\\b999\\b"))
    if (nrow(ed_sub) == 0) {
      message("Skipped ", ds_name, ": no ED records."); next
    }

    ed_sub <- ed_sub |>
      dplyr::select(SUBJECT_ID, dplyr::all_of(date_col)) |>
      dplyr::mutate(!!date_col := as.Date(.data[[date_col]])) |>
      dplyr::distinct()
    new_col_name <- paste0(ds_name, "_ED")
    names(ed_sub)[names(ed_sub) == date_col] <- new_col_name
    ed_sub <- dplyr::mutate(ed_sub, dplyr::across(new_col_name, as.character))

    ed_data_list[[ds_name]] <- ed_sub
  }

  ## Merge ds6001 ED with other datasets
  ED_OTHERS <- if (length(ed_data_list) > 0) {
    purrr::reduce(ed_data_list, dplyr::full_join, by = "SUBJECT_ID")
  } else {
    tibble::tibble(SUBJECT_ID = ED_DS$SUBJECT_ID)
  }

  ED_full <- dplyr::full_join(ED_DS, ED_OTHERS, by = "SUBJECT_ID") |>
    dplyr::distinct(SUBJECT_ID, .keep_all = TRUE)

  ed_cols <- setdiff(names(ED_full), c("SUBJECT_ID", "SITEID"))

  ED_cleaned <- ED_full |>
    dplyr::mutate(dplyr::across(dplyr::all_of(ed_cols), as.Date)) |>
    dplyr::filter(!(dplyr::if_all(dplyr::all_of(ed_cols), ~ !is.na(.)) &
                      apply(dplyr::select(., dplyr::all_of(ed_cols)), 1,
                            function(x) length(unique(x)) == 1))) |>
    dplyr::mutate(Issue_type = "Automatic",
                  PPD_Comment_or_resolution = "",
                  Status = "New") |>
    convert_dates_to_char()

  ## Add SITEID back
  SITEID_lookup <- ds6001 |>
    dplyr::select(SUBJECT_ID, SITEID) |>
    dplyr::distinct()
  ED_cleaned <- ED_cleaned |>
    dplyr::select(-SITEID) |>
    dplyr::left_join(SITEID_lookup, by = "SUBJECT_ID") |>
    dplyr::relocate(SITEID, .after = SUBJECT_ID)

  ## Write into workbook
  openxlsx::addWorksheet(wb, output_tab, tabColour = "#FFFF99")
  openxlsx::writeData(wb, sheet = output_tab, x = ED_cleaned)

  invisible(NULL)
}

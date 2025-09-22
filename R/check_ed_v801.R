#' Check ED vs V801 consistency
#'
#' This function checks two issues:
#' \enumerate{
#'   \item For subjects having both ED and V801, ED date is on or after V801
#'   \item Regulatory visits after ED across all datasets (excluding diary data)
#' }
#'
#' @param datasets_pool Named list of raw datasets
#' @param wb An openxlsx Workbook object
#' @param dataset_no Character vector of datasets to exclude (e.g., diary datasets)
#' @param visit_info_df Metadata with dataset, visit_label_col, visit_date_col
#' @param output_tab Name of Excel worksheet to create
#'
#' @return Writes issues into \code{wb}
#' @keywords internal
check_ed_v801 <- function(datasets_pool, wb,
                          dataset_no = NULL,
                          visit_info_df = NULL,
                          output_tab = NULL) {

  non_diary <- setdiff(names(datasets_pool), dataset_no)
  if (length(non_diary) == 0) {
    warning("ED and V801 check skipped: no non-diary datasets")
    return(invisible(NULL))
  }

  ## collect visits
  ed_v801_list <- list()
  for (ds in non_diary) {
    df <- datasets_pool[[ds]]
    info_row <- dplyr::filter(visit_info_df, dataset == ds)

    if (nrow(info_row) == 0) {
      warning("Dataset ", ds, " not found in visit_info_df. Skipped."); next
    }

    date_col  <- info_row$visit_date_col
    visit_col <- info_row$visit_label_col
    if (is.na(date_col) || is.na(visit_col)) {
      warning("Dataset ", ds, " missing date/visit col info. Skipped."); next
    }

    tmp <- df |>
      dplyr::transmute(
        dataset    = ds,
        SUBJECT_ID = as.character(SUBJECT_ID),
        VISIT      = .data[[visit_col]],
        VISIT_DATE = as.Date(.data[[date_col]])
      ) |>
      dplyr::filter(!is.na(VISIT_DATE)) |>
      dplyr::mutate(
        VISIT_TYPE = dplyr::case_when(
          stringr::str_detect(VISIT, regex("\\b(?:EVV?\\s*801|V\\s*801|VISIT\\s*801|801)\\b", ignore_case = TRUE)) ~ "V801",
          stringr::str_detect(VISIT, regex("^ED$|EARLY DISCONTINUATION", ignore_case = TRUE)) ~ "ED",
          stringr::str_detect(VISIT, regex("\\b(?:EVV?\\s*([1-9]|10)|V\\s*([1-9]|10)|VISIT\\s*0*([1-9]|10)|0*([1-9]|10))\\b", ignore_case = TRUE)) ~ "REG",
          TRUE ~ "OTHER"
        )
      ) |>
      dplyr::distinct(dataset, SUBJECT_ID, VISIT, VISIT_DATE, VISIT_TYPE)

    ed_v801_list[[ds]] <- tmp
  }

  visit_all <- dplyr::bind_rows(ed_v801_list)
  if (nrow(visit_all) == 0) {
    warning("ED vs V801 check skipped: visit data not found.")
    return(invisible(NULL))
  }

  if (!any(visit_all$VISIT_TYPE %in% c("ED","V801"))) {
    warning("Skipped ED vs V801 check: no ED or V801 visits.")
    return(invisible(NULL))
  }

  ## ---- Issue 1: ED >= V801 ----
  date_lookup <- visit_all |>
    dplyr::filter(VISIT_TYPE %in% c("ED","V801")) |>
    dplyr::group_by(SUBJECT_ID, VISIT_TYPE) |>
    dplyr::summarise(min_date = min(VISIT_DATE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = VISIT_TYPE, values_from = min_date)

  issue1 <- date_lookup |>
    dplyr::filter(!is.na(ED) & !is.na(V801) & ED >= V801) |>
    dplyr::mutate(

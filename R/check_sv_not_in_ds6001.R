#' Check SV1001 visits not present in DS6001
#'
#' This function checks for regulatory visits (1–28, 801, 802) recorded in
#' \code{sv1001} but not in \code{ds6001}.
#'
#' @param datasets_pool Named list of raw datasets
#' @param wb An openxlsx Workbook object
#' @param other_datasets Not used (kept for compatibility)
#' @param output_tab Name of Excel worksheet to create
#' @param visit_info_df Metadata with dataset, visit_label_col, visit_date_col
#' @param sv_visit Numeric vector of VISIT numbers to check (default: \code{c(1:28, 801, 802)})
#'
#' @return Writes a worksheet with detected issues into \code{wb}
#' @export
check_sv_not_in_ds6001 <- function(datasets_pool, wb, other_datasets = NULL,
                                   output_tab = NULL, visit_info_df = NULL,
                                   sv_visit = c(1:28, 801, 802)) {

  ## required datasets
  req_ds <- c("sv1001", "ds6001")
  missing_ds <- setdiff(req_ds, names(datasets_pool))
  if (length(missing_ds) > 0) {
    warning("Skip SV1001 vs DS6001 check: Missing dataset(s) ",
            paste(missing_ds, collapse = ", "))
    return(invisible(NULL))
  }

  if (is.null(visit_info_df) ||
      !all(c("dataset", "visit_label_col") %in% names(visit_info_df))) {
    warning("Skip check: visit_info_df missing required columns: dataset, visit_label_col")
    return(invisible(NULL))
  }

  sv1001 <- datasets_pool[["sv1001"]]
  ds6001 <- datasets_pool[["ds6001"]]

  sv_visit_label_var <- visit_info_df |>
    dplyr::filter(dataset == "sv1001") |>
    dplyr::pull(visit_label_col) |> as.character()

  sv_visit_date_var <- visit_info_df |>
    dplyr::filter(dataset == "sv1001") |>
    dplyr::pull(visit_date_col) |> as.character()

  ds_visit_label_var <- visit_info_df |>
    dplyr::filter(dataset == "ds6001") |>
    dplyr::pull(visit_label_col) |> as.character()

  ## check visit label columns
  if (length(sv_visit_label_var) == 0 || is.na(sv_visit_label_var) ||
      !(sv_visit_label_var %in% colnames(sv1001))) {
    warning("Skip SV1001 vs DS6001 check: Missing visit label col in sv1001")
    return(invisible(NULL))
  }
  if (length(ds_visit_label_var) == 0 || is.na(ds_visit_label_var) ||
      !(ds_visit_label_var %in% colnames(ds6001))) {
    warning("Skip SV1001 vs DS6001 check: Missing visit label col in ds6001")
    return(invisible(NULL))
  }

  ## required column checker
  check_required <- function(df, need, nm) {
    miss <- setdiff(need, colnames(df))
    if (length(miss)) {
      warning(paste0("Skip SV1001 vs DS6001 check: Missing col(s) in ",
                     nm, ": ", paste(miss, collapse = ", ")))
      return(FALSE)
    }
    return(TRUE)
  }
  if (!check_required(sv1001, c("SUBJECT_ID", sv_visit_label_var, "VISITOCCUR"), "sv1001")) return(invisible(NULL))
  if (!check_required(ds6001, c("SUBJECT_ID", ds_visit_label_var, "FORMEID", "DSSTDAT"), "ds6001")) return(invisible(NULL))

  ## construct regex pattern for visits
  pattern_nums <- paste0(sv_visit, collapse = "|")
  pattern <- paste0("^evV(", pattern_nums, ")$")

  sv_filtered <- sv1001 |>
    dplyr::filter(
      grepl(pattern, .data[[sv_visit_label_var]], ignore.case = TRUE),
      VISITOCCUR == "Y"
    ) |>
    dplyr::select(SUBJECT_ID, SITEID, dplyr::all_of(sv_visit_label_var)) |>
    dplyr::distinct()

  ds_filtered <- ds6001 |>
    dplyr::filter(
      grepl(pattern, .data[[ds_visit_label_var]], ignore.case = TRUE)
    ) |>
    dplyr::select(SUBJECT_ID, SITEID, dplyr::all_of(ds_visit_label_var)) |>
    dplyr::distinct()

  sv_filtered <- dplyr::rename(sv_filtered, EVENT = dplyr::all_of(sv_visit_label_var))
  ds_filtered <- dplyr::rename(ds_filtered, EVENT = dplyr::all_of(ds_visit_label_var))

  missing_visits <- dplyr::anti_join(sv_filtered, ds_filtered,
                                     by = c("SUBJECT_ID", "EVENT")) |>
    dplyr::arrange(SUBJECT_ID) |>
    dplyr::mutate(
      Issue_type = "Automatic",
      Issue_noted_by_Lilly_Stats = paste0("Patient had ", EVENT, " in SV1001, but not in DS6001"),
      PPD_Comment_or_resolution = "",
      Status = "New"
    )

  ## write to workbook
  openxlsx::addWorksheet(wb, output_tab, tabColour = "#FFFF99")
  openxlsx::writeData(wb, sheet = output_tab, x = missing_visits)

  invisible(NULL)
}

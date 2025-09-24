#' Check visit discrepancies across EX/EC, SV, and TRTDSPN
#'
#' This function compares visits across \code{ec1001} (or EX), \code{sv1001},
#' and \code{trtdspn} datasets by VISID, to identify:
#' \enumerate{
#'   \item Missing visit records in one or more datasets
#'   \item Visit date mismatches between TRTDSPN and EX/SV
#' }
#'
#' @param datasets_pool Named list of raw datasets
#' @param wb An openxlsx Workbook object
#' @param seq Integer. If 1, run missing visits check; if 2, run date mismatch check.
#' @param output_tab Name of Excel worksheet to create
#' @param visit_info_df Metadata with dataset, visit_label_col, visit_date_col
#' @param visit_excl Numeric vector of VISIDs to exclude (default: \code{c(1,28,801,802,803)})
#'
#' @return Writes a worksheet with detected issues into \code{wb}
#' @export
check_visit_discrepancy <- function(datasets_pool, wb,
                                    seq = NULL,
                                    output_tab = NULL,
                                    visit_info_df = NULL,
                                    visit_excl = c(1,28,801,802,803)) {

  ds_ex  <- "ec1001"    # sometimes EX1001
  ds_sv  <- "sv1001"
  ds_trt <- "trtdspn"

  ec1001   <- datasets_pool[[ds_ex]]
  sv1001   <- datasets_pool[[ds_sv]]
  trtdspn  <- datasets_pool[[ds_trt]]
  ds6001   <- datasets_pool[["ds6001"]]

  ## find visit label/date col from spec
  ex_label_var <- dplyr::filter(visit_info_df, dataset == ds_ex) |> dplyr::pull(visit_label_col)
  ex_date_var  <- dplyr::filter(visit_info_df, dataset == ds_ex) |> dplyr::pull(visit_date_col)
  sv_label_var <- dplyr::filter(visit_info_df, dataset == ds_sv) |> dplyr::pull(visit_label_col)
  sv_date_var  <- dplyr::filter(visit_info_df, dataset == ds_sv) |> dplyr::pull(visit_date_col)

  ## sanity checks
  if (is.na(ex_label_var) || !(ex_label_var %in% colnames(ec1001)) ||
      is.na(ex_date_var)  || !(ex_date_var %in% colnames(ec1001))) {
    warning("Skipped visit check: ec1001 missing required columns.")
    return(invisible(NULL))
  }
  if (is.na(sv_label_var) || !(sv_label_var %in% colnames(sv1001)) ||
      is.na(sv_date_var)  || !(sv_date_var %in% colnames(sv1001))) {
    warning("Skipped visit check: sv1001 missing required columns.")
    return(invisible(NULL))
  }

  ## standardize ec1001
  ex_clean <- ec1001 |>
    dplyr::select(SUBJECT_ID, SITEID, dplyr::all_of(ex_label_var), dplyr::all_of(ex_date_var), ECOCCUR) |>
    dplyr::mutate(
      VISID = as.numeric(stringr::str_extract(.data[[ex_label_var]], "\\d+")),
      date = .data[[ex_date_var]],
      SUBJECT_ID = as.character(SUBJECT_ID)
    ) |>
    dplyr::rename(event_ex = dplyr::all_of(ex_label_var)) |>
    dplyr::select(SUBJECT_ID, VISID, date, event_ex, ECOCCUR)

  ## standardize sv1001
  sv_clean <- sv1001 |>
    dplyr::select(SUBJECT_ID, dplyr::all_of(sv_label_var), dplyr::all_of(sv_date_var)) |>
    dplyr::mutate(
      VISID = as.numeric(stringr::str_extract(.data[[sv_label_var]], "\\d+")),
      date = .data[[sv_date_var]],
      SUBJECT_ID = as.character(SUBJECT_ID)
    ) |>
    dplyr::rename(event_sv = dplyr::all_of(sv_label_var)) |>
    dplyr::select(SUBJECT_ID, VISID, date, event_sv)

  ## standardize trtdspn
  trt_clean <- trtdspn |>
    dplyr::select(SUBJECT_ID, PKGDTTM, VISID) |>
    dplyr::mutate(
      date = as.Date(PKGDTTM),
      SUBJECT_ID = as.character(SUBJECT_ID)
    ) |>
    dplyr::distinct(SUBJECT_ID, VISID, date, .keep_all = TRUE)

  ## merge
  merged <- dplyr::full_join(ex_clean, sv_clean, by = c("SUBJECT_ID", "VISID", "date")) |>
    dplyr::full_join(dplyr::select(trt_clean, -date), by = c("SUBJECT_ID", "VISID"))

  ## exclusion: non-randomized subjects
  non_randomized_subjects <- ds6001 |>
    dplyr::filter(DSCONT_RANDTRT == "N") |>
    dplyr::pull(SUBJECT_ID) |>
    as.character()

  ## (1) Missing visits
  visit_issues1 <- merged |>
    dplyr::filter(!VISID %in% visit_excl) |>
    dplyr::filter(is.na(ECOCCUR) | ECOCCUR == "Y") |>
    dplyr::filter(is.na(event_ex) | is.na(event_sv) | is.na(VISID) | is.na(PKGDTTM)) |>
    dplyr::filter(!SUBJECT_ID %in% non_randomized_subjects) |>
    dplyr::rowwise() |>
    dplyr::mutate(Issue_type = "Automatic",
                  Issue_noted_by_Lilly_Stats = {
                    missing_fields <- c()
                    if (is.na(event_ex)) missing_fields <- c(missing_fields, "EC1001")
                    if (is.na(event_sv)) missing_fields <- c(missing_fields, "SV1001")
                    if (is.na(PKGDTTM))  missing_fields <- c(missing_fields, "TRTDSPN")
                    if (is.na(VISID))    missing_fields <- c(missing_fields, "VISID")
                    paste0("Missing visit(s) in ", paste(missing_fields, collapse = ", "))
                  },
                  PPD_Comment_or_resolution = "",
                  Status = "New") |>
    dplyr::ungroup() |>
    dplyr::arrange(SUBJECT_ID, VISID, date) |>
    convert_dates_to_char()

  ## (2) Date discrepancy
  visit_issues2 <- merged |>
    dplyr::filter(!VISID %in% visit_excl) |>
    dplyr::filter(!ECOCCUR %in% c("", "N")) |>
    dplyr::filter(event_sv != "Unscheduled visit") |>
    dplyr::filter(!SUBJECT_ID %in% non_randomized_subjects) |>
    dplyr::rowwise() |>
    dplyr::filter(
      is.na(event_ex) | is.na(event_sv) | is.na(VISID) | is.na(PKGDTTM) |
        (!is.na(date) & !is.na(PKGDTTM) & date != as.Date(PKGDTTM))
    ) |>
    dplyr::mutate(
      Issue_type = "Automatic",
      Issue_noted_by_Lilly_Stats = "PKGDTTM (in TRTDSPN) later than EX/SV visit date",
      PPD_Comment_or_resolution = "",
      Status = "New"
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(SUBJECT_ID, VISID, date)

  ## Write results
  if (seq == 1) {
    openxlsx::addWorksheet(wb, output_tab, tabColour = "#FFFF99")
    openxlsx::writeData(wb, sheet = output_tab, x = visit_issues1)
  }
  if (seq == 2) {
    openxlsx::addWorksheet(wb, output_tab, tabColour = "#FFFF99")
    openxlsx::writeData(wb, sheet = output_tab, x = visit_issues2)
  }

  invisible(NULL)
}

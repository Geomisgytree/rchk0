#' rchk0_reference
#'
#' Curated list of exported functions, grouped by topic.
#'
#' @section Common visit checks (applied to all datasets):
#' - [check_missing_key_vars()] - Check observations that having missing visit numbers (e.g., VISITNUM)
#' - [check_duplicate_visit_date()] - Check duplicate visits on the same date (e.g., Visit 14 and Visit 15 on the same date 2025-07-21)
#' - [check_visit_after_ED()] — Visits after an ED date
#' - [check_visit_overlap()] — detect overlapped visits
#'
#' @section Export & reporting:
#' - [export_excel()] — write formatted issue logs
#'
#' @section Core workflow:
#' - [run_tracker()] — run all standard checks and export
#'
#' @section Utilities:
#' - [compress_ranges()], [convert_dates_to_char()], [issue_envelope()]
#'
#' @name rchk0_reference
NULL

#' Check subjects belonging to multiple sites
#'
#' This function identifies subjects who appear in more than one site across datasets.
#'
#' @param datasets_pool Named list of raw datasets
#' @param wb An openxlsx Workbook object
#' @param output_tab Name of Excel worksheet to create
#' @param visit_info_df Metadata with dataset, subject_var, site_var
#'
#' @return Writes a worksheet with detected issues into \code{wb}
#' @keywords internal
check_multi_site <- function(datasets_pool, wb, output_tab = NULL,
                             visit_info_df = NULL) {

  available_ds <- names(datasets_pool)

  multi_site_summary <- purrr::map_dfr(available_ds, function(ds_name) {
    df <- datasets_pool[[ds_name]]
    info_row <- dplyr::filter(visit_info_df, dataset == ds_name)

    if (nrow(info_row) == 0) {
      warning("Dataset ", ds_name, " not found in visit_info_df. Skipped.")
      return(NULL)
    }

    subject_col <- info_row$subject_var
    site_col    <- info_row$site_var

    if (is.na(subject_col) || is.na(site_col)) {
      warning("Dataset ", ds_name, " missing subject/site column info. Skipped.")
      return(NULL)
    }

    df |>
      dplyr::filter(!is.na(SITEID), !is.na(SUBJECT_ID)) |>
      dplyr::group_by(SUBJECT_ID) |>
      dplyr::summarise(
        n_sites   = dplyr::n_distinct(SITEID),
        site_list = paste(sort(unique(SITEID)), collapse = ", "),
        .groups   = "drop"
      ) |>
      dplyr::filter(n_sites > 1) |>
      dplyr::transmute(
        SUBJECT_ID,
        SITEID       = site_list,
        dataset_name = ds_name
      )
  })

  if (nrow(multi_site_summary) == 0) {
    message("No subjects in multiple sites found.")
    return(invisible(NULL))
  }

  issues <- multi_site_summary |>
    dplyr::mutate(
      Issue_type = "Automatic",
      Issue_noted_by_Lilly_Stats = "Subjects in multiple sites",
      PPD_Comment_or_resolution = "",
      Status = "New"
    )

  openxlsx::addWorksheet(wb, output_tab, tabColour = "#FFFF99")
  openxlsx::writeData(wb, sheet = output_tab, x = issues)

  invisible(NULL)
}

# R/preprocess.R
#' Preprocess raw datasets according to spec
#' @param datasets_pool Named list returned by load_dataset()
#' @param preprocess_spec Data frame from spec sheet PREPROCESSING
#' @return list(visit_info_df=..., datasets_pool=...)
#' @keywords internal
preprocess_data <- function(datasets_pool, preprocess_spec) {
  visit_info_list <- vector("list", length(datasets_pool))
  ii <- 0

  for (nm in names(datasets_pool)) {
    df <- datasets_pool[[nm]]
    spec_row <- preprocess_spec |>
      dplyr::filter(DATASET == nm & (is.na(REMOVE) | REMOVE != "Y"))

    # 1) SUBJECT_ID
    if ("SUBJECT" %in% names(df)) {
      df <- dplyr::mutate(df, SUBJECT_ID = sub(".*-", "", .data$SUBJECT)); subject_var <- "SUBJECT"
    } else if ("SUBJID" %in% names(df)) {
      df <- dplyr::mutate(df, SUBJECT_ID = sub(".*-", "", .data$SUBJID)); subject_var <- "SUBJID"
    } else if ("USUBJID" %in% names(df)) {
      df <- dplyr::mutate(df, SUBJECT_ID = sub(".*-", "", .data$USUBJID)); subject_var <- "USUBJID"
    } else if ("SUBJECTNAME" %in% names(df)) {
      df <- dplyr::mutate(df, SUBJECT_ID = sub(".*-", "", .data$SUBJECTNAME)); subject_var <- "SUBJECTNAME"
    } else {
      df <- dplyr::mutate(df, SUBJECT_ID = NA_character_); subject_var <- ""
    }

    # 2) SITEID
    if ("SITEID" %in% names(df)) {
      df <- dplyr::mutate(df, SITEID = as.character(.data$SITEID)); site_var <- "SITEID"
    } else if ("SITENUM" %in% names(df)) {
      df <- dplyr::mutate(df, SITEID = as.character(.data$SITENUM)); site_var <- "SITENUM"
    } else if ("USUBJID" %in% names(df)) {
      df <- dplyr::mutate(df, SITEID = substr(.data$USUBJID, 1, 5)); site_var <- "USUBJID"
    } else if ("SITE" %in% names(df)) {
      df <- dplyr::mutate(df, SITEID = as.character(.data$SITE)); site_var <- "SITE"
    } else if ("SITENUMBER" %in% names(df)) {
      df <- dplyr::mutate(df, SITEID = as.character(.data$SITENUMBER)); site_var <- "SITENUMBER"
    } else if ("INVID" %in% names(df)) {
      df <- dplyr::mutate(df, SITEID = as.character(.data$INVID)); site_var <- "INVID"
    } else {
      df <- dplyr::mutate(df, SITEID = NA_character_); site_var <- ""
    }

    # 3) row filter
    if (nrow(spec_row) > 0 && !is.na(spec_row$EXCLUDE_ROWS)) {
      df <- dplyr::filter(df, eval(parse(text = spec_row$EXCLUDE_ROWS)))
    }

    # 4) column filter
    if (nrow(spec_row) > 0 && !is.na(spec_row$EXCLUDE_COLS)) {
      selected_cols <- strsplit(spec_row$EXCLUDE_COLS, ",")[[1]] |> trimws()
      existing_cols <- selected_cols[selected_cols %in% names(df)]
      if (length(existing_cols)) df <- dplyr::select(df, dplyr::all_of(existing_cols))
    }

    # 5) visit_date_col
    visit_date_col <- NULL
    if ("EVENTDT" %in% names(df)) {
      visit_date_col <- "EVENTDT"
    } else if (nm == "lab" && "LBDTM" %in% names(df)) {
      visit_date_col <- "LBDTM"
    } else if (nm == "sv1001") {
      visit_date_col <- "VISDAT"
    } else if (nm == "vs1001") {
      visit_date_col <- "VSDAT"
    } else if ("ECOAASMDT" %in% names(df)) {
      df <- dplyr::mutate(df, ECOAASMDT = as.Date(.data$ECOAASMDT))
      visit_date_col <- "ECOAASMDT"
    } else if (nm == "ec1001") {
      visit_date_col <- "ECSTDAT"
    }

    # 6) visit_label_col
    visit_label_col <- if ("VISIT" %in% names(df)) "VISIT" else if ("EVENT" %in% names(df)) "EVENT" else NULL

    # 回填
    datasets_pool[[nm]] <- df
    ii <- ii + 1
    visit_info_list[[ii]] <- data.frame(
      dataset = nm,
      subject_var = subject_var,
      site_var = site_var,
      visit_date_col = ifelse(is.null(visit_date_col), NA, visit_date_col),
      visit_label_col = ifelse(is.null(visit_label_col), NA, visit_label_col),
      stringsAsFactors = FALSE
    )
  }

  visit_info_df <- do.call(rbind, visit_info_list); rownames(visit_info_df) <- NULL
  list(visit_info_df = visit_info_df, datasets_pool = datasets_pool)
}

# R/run_tracker.R
#' Run KIAC-style Raw Issue Tracker
#' @param data_path directory of raw datasets
#' @param spec_path xlsx spec path
#' @param out_xlsx output Excel path
#' @param study_name optional study label
#' @param exclude_prefixes prefixes to drop
#' @param exclude_names exact basenames to drop
#' @export
run_tracker <- function(data_path, spec_path, out_xlsx,
                        study_name = NULL,
                        exclude_prefixes = c("cdms"),
                        exclude_names = c("record_count", "rtsm_sbjct_data", "sd", "mhpresp1001")) {

  all_check    <- openxlsx::read.xlsx(spec_path, sheet = "ALL_DATASETS") |> dplyr::filter(is.na(.data$REMOVE))
  simple_check <- openxlsx::read.xlsx(spec_path, sheet = "SINGLE_DATASETS") |> dplyr::filter(is.na(.data$REMOVE))
  preprocess_spec <- openxlsx::read.xlsx(spec_path, sheet = "PREPROCESSING")

  datasets_pool <- load_dataset(data_path,
                                pattern = "\\.sas7bdat$",
                                exclude_prefixes = exclude_prefixes,
                                exclude_names = exclude_names)

  prep <- preprocess_data(datasets_pool, preprocess_spec)
  datasets_pool <- prep$datasets_pool
  data_info <- prep$visit_info_df

  wb <- openxlsx::createWorkbook()
  wb <- create_readme_sheet(wb, study_name = study_name %||% "", all_check = all_check, simple_check = simple_check)

  # ALL_DATASETS status
  for (i in seq_len(nrow(all_check))) {
    fun_name  <- all_check$FUNCTION_NAME[i]
    out_tab   <- all_check$OUTPUT_TAB[i]
    ds_req_sv <- all_check$DATASET_REQUIRED[i]
    param_str <- all_check$PARAMS[i]

    if (!nzchar(fun_name) || !exists(fun_name, mode = "function")) { warning("Function '", fun_name, "' not found; skipping."); next }
    required_ds <- if (!is.na(ds_req_sv) && nzchar(ds_req_sv) && tolower(ds_req_sv) != "all") strsplit(ds_req_sv, ",")[[1]] |> trimws() else NULL
    if (!is.null(required_ds) && !all(required_ds %in% names(datasets_pool))) {
      missing_ds <- setdiff(required_ds, names(datasets_pool))
      warning("Function ", fun_name, " missing dataset(s): ", paste(missing_ds, collapse = ", ")); next
    }

    param_list <- list(
      datasets_pool = datasets_pool,
      wb            = wb,
      visit_info_df = data_info,
      output_tab    = out_tab
    )

    if (!is.na(param_str) && nzchar(param_str)) {
      params <- strsplit(param_str, ",(?=(?![^()]*\\)))", perl = TRUE)[[1]] |> trimws()
      for (p in params) {
        kv <- strsplit(p, "=", fixed = TRUE)[[1]]
        if (length(kv) < 2) next
        key <- trimws(kv[1]); val_expr <- paste(kv[-1], collapse = "=") |> trimws()
        value <- tryCatch(
          if (val_expr %in% names(all_check)) {
            v <- all_check[[val_expr]][i]; if (is.character(v) && grepl(",", v)) strsplit(v, ",")[[1]] |> trimws() else v
          } else eval(parse(text = val_expr)),
          error = function(e) val_expr
        )
        param_list[[key]] <- value
      }
    }

    message("Running: ", fun_name, " → ", out_tab)
    do.call(get(fun_name), param_list)
  }

  # SINGLE_DATASETS
  wb <- append_single_dataset_results(wb = wb, simple_check = simple_check, datasets_pool = datasets_pool)

  wb <- update_issue_log(wb, out_xlsx)
  format_issue_log_sheets(wb)
  openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

  invisible(list(wb = wb, visit_info_df = data_info, datasets_pool = datasets_pool))
}

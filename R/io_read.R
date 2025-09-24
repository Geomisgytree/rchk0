# R/io_read.R
#' Load raw datasets into a named list
#' @param path Directory to search
#' @param pattern Regex for files (default .sas7bdat)
#' @param exclude_prefixes Character vector of prefixes to drop
#' @param exclude_names Exact dataset basenames to drop (without extension)
#' @return Named list of data.frames/tibbles
#' @export
load_dataset <- function(path,
                         pattern = "\\.sas7bdat$",
                         exclude_prefixes = c("sys", "cdms"),
                         exclude_names = c("record_count", "rtsm_sbjct_data")) {
  stopifnot(dir.exists(path))
  all_files <- list.files(path, pattern = pattern, full.names = TRUE)
  dataset_names <- gsub("^.*/|\\.sas7bdat$", "", all_files)

  # exclude
  pref_hit <- Reduce(`|`, lapply(exclude_prefixes, function(pref) {
    grepl(paste0("^", pref), dataset_names, ignore.case = TRUE)
  }))
  keep <- !(pref_hit | dataset_names %in% exclude_names)

  files_keep <- all_files[keep]
  names_keep <- dataset_names[keep]

  out <- vector("list", length(files_keep))
  names(out) <- names_keep
  for (i in seq_along(files_keep)) {
    df <- haven::read_sas(files_keep[i])
    if (nrow(df) > 0) out[[i]] <- df
  }
  out <- out[!vapply(out, is.null, logical(1))]
  message("Datasets load completed! :)")
  out
}

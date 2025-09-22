#' Compress consecutive integers into ranges
#'
#' @param x Integer vector
#' @return A string such as "1-4, 6"
#' @export
compress_ranges <- function(x) {
  x <- sort(as.integer(x))
  if (length(x) == 0) return("")
  breaks <- c(0, which(diff(x) != 1), length(x))
  result <- c()
  for (i in seq_along(breaks)[-length(breaks)]) {
    group <- x[(breaks[i]+1):breaks[i+1]]
    if (length(group) == 1) {
      result <- c(result, as.character(group))
    } else {
      result <- c(result, paste0(group[1], "-", group[length(group)]))
    }
  }
  paste(result, collapse = ", ")
}

#' Safely convert to character while preserving NA
#'
#' @param x A vector
#' @return Character vector with NA unchanged
#' @keywords internal
safe_as_character <- function(x) {
  if (is.character(x)) return(x)
  out <- as.character(x)
  out[is.na(x)] <- NA
  out
}

#' Convert Date/POSIX columns in a data.frame to character
#'
#' @param df A data.frame
#' @param date_format Format for Date columns
#' @param dttm_format Format for POSIXct/POSIXlt columns
#' @return data.frame with converted columns
#' @export
convert_dates_to_char <- function(df, date_format = "%Y-%m-%d", dttm_format = "%Y-%m-%d %H:%M:%S") {
  df[] <- lapply(df, function(x) {
    if (inherits(x, "Date")) {
      format(x, date_format)
    } else if (inherits(x, c("POSIXct", "POSIXlt"))) {
      format(x, dttm_format)
    } else {
      x
    }
  })
  df
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

#' Convert season string to date
#'
#' Converts season string like \code{2010-2011} to a date string like
#' \code{2011-01-01}. Useful when plotting data that has been summarized by
#' season.
#'
#' @param x character; season to convert
#' @export
season_to_date <- function(x) {
  paste0(substr(x, 6, 9), "-01-01")
}

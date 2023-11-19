#' Add Collapsed Integer Days For Plotting
#'
#' Adds an integer column \code{.plot_day} to a data frame of results that
#' represents a sequential time series, but with summer gaps removed.
#' Any results that actually occur during summer (Southern Hemisphere) will
#' be \code{NA}. Also adds attributes to the data frame called
#' \code{x_breaks} and \code{x_labels} that can be used when plotting to mark
#' the first of each year (Jan 1st).
#'
#' @param data data frame of results; must contain \code{season} and \code{date}
#' columns
#' @export
add_plot_day <- function(data){
  if (!all(c("date","season") %in% colnames(data))){
    stop("Data must have both date and season columns.")
  }

  grp_cols <- group_vars(data)

  date_rng_by_season <- data |>
    group_by_at(.vars = grp_cols) |>
    summarise(min_date = min(date,na.rm = TRUE),
              max_date = max(date,na.rm = TRUE)) |>
    #summarise(min_date = paste0(substr(season[1],1,4),"-10-01"),
    #          max_date = paste0(substr(season[1],6,9),"-05-01")) |>
    group_by_at(.vars = setdiff(grp_cols,"season")) |>
    mutate(.gap_days = as.Date(lead(min_date,1)) - as.Date(max_date),
           .gap_days = c(0,head(as.integer(.gap_days),-1)),
           .gap_days = cumsum(.gap_days),
           .ref_date = season_to_date(season)) |>
    select(-min_date,-max_date) |>
    ungroup() |>
    arrange_at(.vars = rev(grp_cols))

  data <- data |>
    arrange(date) |>
    left_join(select(date_rng_by_season,-.ref_date),by = grp_cols) |>
    mutate(.plot_day = as.integer(as.Date(date)) - .gap_days,
           .plot_day = if_else(substr(date,6,7) %in% c('05','06','07','08','09'),
                               NA_real_,.plot_day))
  x_labs <- date_rng_by_season |>
    mutate(.ref_days = as.integer(as.Date(.ref_date)) - .gap_days)

  attr(data,"x_breaks") <- x_labs$.ref_days
  attr(data,"x_labels") <- substr(x_labs$.ref_date,1,4)

  return(data)
}

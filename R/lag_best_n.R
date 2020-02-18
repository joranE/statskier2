#' Lagged Best N Values
#'
#' Calculates a function (\code{.f}) of the top \code{best_n} values
#' from \code{x} over the previous \code{window} days. Assumes that \code{x}
#' and \code{index} are already in order according to \code{index}.
#'
#' This is done entirely in R, so it can be quite slow on large data sets.
#'
#' @param x numeric vector of values
#' @param index dates, assumed to be character in YYYY-MM-DD format
#' @param best_n integer
#' @param window integer; previous number of days to examine
#' @param adj vector of adjustments for when there are fewer than \code{best_n}
#' values.
#' @export
lag_best_n <- function(x,index,best_n = 5,
                       window = 365,.f = mean,
                       adj = c(1.4,1.3,1.2,1.1)){
  end <- as.integer(as.Date(index))
  start <- end - window
  out <- rep(NA,length(x))

  for (i in seq_along(x)){
    idx <- which(end <= end[i] & end >= start[i])
    x_vals <- x[idx]
    x_vals <- x_vals[!is.na(x_vals)]
    n <- length(x_vals)
    if (n <= 4) {
      out[i] <- .f(x_vals) * adj[n]
    } else{
      out[i] <- .f(head(sort(x_vals),n = best_n))
    }
  }
  return(out)
}

#' Lagged Average of Top Results
#'
#' FIS points are typically the average of the best 5 results over the prior
#' calendar year. This is a fast, Rcpp way to replicate this calculation.
#'
#' @param x numeric vector of values
#' @param dates dates, assumed to be character in YYYY-MM-DD format
#' @param n integer
#' @param window integer; previous number of days to examine
#' @param adj vector of adjustments for when there are fewer than \code{n}
#' values.
#' @export
lag_best_avg <- function(x,dates,n = 5L,window = 365L,adj = c(1.4,1.3,1.2,1.1,1)){
  dates <- as.integer(as.Date(dates))
  lagAvgTopN(values = x,endDates = dates,n = n,window = window,adj = adj)
}

#' Lagged Median All Results
#'
#' Median result over the past calendar window in days.
#'
#' @param x numeric vector of values
#' @param dates dates, assumed to be character in YYYY-MM-DD format
#' @param n integer
#' @param window integer; previous number of days to examine
#' @param adj vector of adjustments for when there are fewer than \code{n}
#' values.
#' @export
lag_median <- function(x,dates,window = 365L,adj = c(1.4,1.3,1.2,1.1,1)){
  dates <- as.integer(as.Date(dates))
  lagMedianAll(values = x,endDates = dates,window = window,adj = adj)
}

#' Lagged Median All Results
#'
#' Median result over the past calendar window in days.
#'
#' @param x numeric vector of values
#' @param dates dates, assumed to be character in YYYY-MM-DD format
#' @param n integer
#' @param window integer; previous number of days to examine
#' @param adj vector of adjustments for when there are fewer than \code{n}
#' values.
#' @export
lag_mad <- function(x,dates,window = 365L){
  dates <- as.integer(as.Date(dates))
  lagMAD(values = x,endDates = dates,window = window)
}

#' Lagged Median All Results
#'
#' Median result over the past calendar window in days.
#'
#' @param x numeric vector of values
#' @param dates dates, assumed to be character in YYYY-MM-DD format
#' @param n integer
#' @param window integer; previous number of days to examine
#' @param adj vector of adjustments for when there are fewer than \code{n}
#' values.
#' @export
lag_sd <- function(x,dates,window = 365L){
  dates <- as.integer(as.Date(dates))
  lagSD(values = x,endDates = dates,window = window)
}

#' FIS Point Threshold Necessary to Raise/Lower Athlete's Point
#'
#' Calculate the point threshold at any given point in time at which an athlete
#' would be able to lower thair carried FIS points.
#'
#' @param x numeric vector of FIS points
#' @param dates character vector of dates, must be in YYYY-MM-DD format
#' @param window integer number of days
#' @export
pts_thresh <- function(x,dates,window = 365L){
  dates <- as.integer(as.Date(dates))
  ptsThresh(values = x,endDates = dates,window = window)
}

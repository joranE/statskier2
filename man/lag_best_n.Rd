% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lag_best_n.R
\name{lag_best_n}
\alias{lag_best_n}
\title{Lagged Best N Values}
\usage{
lag_best_n(x, index, best_n = 5, window = 365, .f = mean, adj = c(1.4,
  1.3, 1.2, 1.1))
}
\arguments{
\item{x}{numeric vector of values}

\item{index}{dates, assumed to be character in YYYY-MM-DD format}

\item{best_n}{integer}

\item{window}{integer; previous number of days to examine}

\item{adj}{vector of adjustments for when there are fewer than \code{best_n}
values.}
}
\description{
Calculates a function (\code{.f}) of the top \code{best_n} values
from \code{x} over the previous \code{window} days. Assumes that \code{x}
and \code{index} are already in order according to \code{index}.
}

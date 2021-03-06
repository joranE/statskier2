% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boot_funs.R
\name{bs_top_n}
\alias{bs_top_n}
\title{Bootstrap Average of Top n Values}
\usage{
bs_top_n(vec, n = 5, B = 1000)
}
\arguments{
\item{vec}{vector of values}

\item{n}{integer; select n best (lowest) values}

\item{B}{integer; number of bootstrap replicates}
}
\value{
A list with components \code{orig} with the original statistic and
\code{bs} a vector of the bootstrapped statistics
}
\description{
Bootstrap the mean of the best (lowest) n values in a vector.
}

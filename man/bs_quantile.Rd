% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/boot_funs.R
\name{bs_quantile}
\alias{bs_quantile}
\title{Bootstrap Quantiles}
\usage{
bs_quantile(vec, probs, B = 1000)
}
\arguments{
\item{vec}{vector of values}

\item{probs}{vector of values between 0 and 1; quantiles to estimate}

\item{B}{integer; number of bootstrap replicates}
}
\value{
A list with components \code{orig} with the original statistic and
\code{bs} a matrix whose columns are the bootstrapped statistics
}
\description{
Bootstrap quantiles of a vector.
}

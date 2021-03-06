% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/median_perc_back.R
\name{mpb}
\alias{mpb}
\title{Raw Median Percent Back}
\usage{
mpb(race_data)
}
\arguments{
\item{race_data}{data frame with race results which must contain
columns \code{time} and \code{raceid}.}
}
\value{
The original data frame with a new column called \code{mpb}.
}
\description{
Calculate the raw percent back from the median skier
}
\examples{
\dontrun{
library(dplyr)
conr <- db_xc_remote()
dat <- ss_query(conr,"select * from main where raceid = 7902")
dat \%>\% mpb()
}
}
\seealso{
\code{\link{standardize_mpb}}
}

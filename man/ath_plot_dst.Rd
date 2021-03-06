% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ath_plot.R
\name{ath_plot_dst}
\alias{ath_plot_dst}
\alias{ath_plot_spr}
\title{Sprint & Distance Results Plots By Athlete}
\usage{
ath_plot_dst(ath_names, races = c("maj_int", "fis"), by_tech = FALSE,
  collapse = NULL, use_rank = FALSE)

ath_plot_spr(ath_names, races = c("maj_int", "fis"), by_tech = FALSE,
  collapse = NULL, use_rank = TRUE)
}
\arguments{
\item{ath_names}{character vector of athlete names}

\item{races}{character; either "maj_int" for only major international
results or "fis" for all results}

\item{by_tech}{boolean; if \code{TRUE} summarise by both season and technique,
rather than just season}

\item{collapse}{named list of athletes to collapse into a single panel}

\item{use_rank}{boolean; if \code{TRUE} use rank (finishing place) instead
of median percent back or FIS points when races = "maj_int".}
}
\value{
List with three components,
 \enumerate{
   \item \code{plot} - ggplot2 plot object
   \item \code{ath_data} - raw athlete data
   \item \code{ath_summary} - athlete data summarised by season/technique
 }
}
\description{
Faceted plots of results over time for one or several athletes using
either FIS points, median percent back or finishing place as
a measure of performance. Includes summaries by season or by season and
technique.
}
\examples{
\dontrun{
p <- ath_plot_dst(ath_names = c('DIGGINS Jessica','BJORNSEN Sadie','CALDWELL Sophie'),
                  races = "maj_int")
print(p$plot)
}
}

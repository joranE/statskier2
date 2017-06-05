#' Raw Median Percent Back
#'
#' Calculate the raw percent back from the median skier
#'
#' @param race_data data frame with race results which must contain
#' columns \code{time} and \code{raceid}.
#' @return The original data frame with a new column called \code{mpb}.
#' @import dplyr
#' @export
#' @seealso \code{\link{standardize_mpb}}
#' @examples
#' \dontrun{
#' library(dplyr)
#' conr <- db_xc_remote()
#' dat <- ss_query(conr,"select * from main where raceid = 7902")
#' dat %>% mpb()
#' }
mpb <- function(race_data){
  if (!all(c('raceid','time') %in% colnames(race_data))){
    stop("\nError: Requires time, raceid columns.")
  }

  races <- unique(race_data$raceid)
  if (length(races) == 1){
    races <- c(races,races)
  }

  race_medians <- tbl(src = options()$statskier_src,from = "median_race_time") %>%
    filter(raceid %in% races) %>%
    collect()

  res <- race_data %>%
    left_join(race_medians,by = "raceid") %>%
    mutate(mpb = round((time - median_time) / median_time,6) * 100) %>%
    select(-median_time) %>%
    as.data.frame()

  res
}

#' Standardized Median Percent Back
#'
#' Standardize median percent back by
#' season, gender and race type
#'
#' @param race_data data frame containing the columns \code{gender},
#' \code{start}, \code{season} and \code{mpb}
#' @return The original data frame with the \code{mpb} column replaced
#' by standardized values, to be on a common scale.
#' @export
#' @seealso \code{\link{mpb}}
#' @examples
#' \dontrun{
#' library(dplyr)
#' conr <- db_xc_remote()
#' dat <- ss_query(conr,"select * from main where raceid = 7902")
#' dat %>% mpb() %>% standardize_mpb()
#' }
standardize_mpb <- function(race_data){
  if (!all(c('mpb','gender','start','season') %in% colnames(race_data))){
    stop("\nRequires mpb,gender,season,start columns.")
  }

  conv_fac <- tbl(src = options()$statskier_src,from = "xc_fac") %>%
    collect()
  res <- race_data %>%
    left_join(conv_fac,by = c('season','gender','start')) %>%
    mutate(mpb = round((mpb - mu) / sigma,4)) %>%
    select(-mu,-sigma) %>%
    as.data.frame()
  res
}

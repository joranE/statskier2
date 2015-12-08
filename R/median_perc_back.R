#' Median Percent Back
#'
#' Calculate the percent back from the median skier
#'
#' @param race_data data frame with race results
#' @import dplyr
#' @export
mpb <- function(race_data){
  if (!all(c('raceid','time') %in% colnames(race_data))){
    stop("\nError: Requires time, raceid columns.")
  }
  src <- src_sqlite(path = statskier2::sqlite_path,create = FALSE)
  races <- unique(race_data$raceid)
  if (length(races) == 1){
    races <- c(races,races)
  }

  race_medians <- tbl(src = src,from = "median_race_time") %>%
    filter(raceid %in% races) %>%
    collect()

  res <- race_data %>%
    left_join(race_medians,by = "raceid") %>%
    mutate(mpb = round((time - median_time) / median_time,6) * 100) %>%
    select(-median_time) %>%
    as.data.frame()
  res
}

#' Standardize Median Percent Back
#'
#' Standardize by season, gender and race type
#'
#' @param race_data
#' @export
standardize_mpb <- function(race_data){
  if (!all(c('mpb','gender','start','season') %in% colnames(race_data))){
    stop("\nRequires mpb,gender,season and start columns.")
  }
  src <- src_sqlite(path = statskier2::sqlite_path,create = FALSE)
  conv_fac <- tbl(src = src,from = "xc_fac") %>%
    collect()
  res <- race_data %>%
    left_join(conv_fac,by = c('season','gender','start')) %>%
    mutate(mpb = round((mpb - mu) / sigma,4)) %>%
    select(-mu,-sigma) %>%
    as.data.frame()
  res
}

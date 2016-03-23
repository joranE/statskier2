#' Recent Races
#'
#' Utility function for listing the most recent
#' races in the database since a given date. Used mainly
#' to help track what new races need to be entered.
#'
#' @param cur_date character; a date in the format "YYYY-MM-DD"
#' @return A data frame with one row for each race listing various details
#' @export
#' @examples
#' \dontrun{
#' x <- recent_races("2016-02-01")
#' head(x)
#' }
recent_races <- function(cur_date){
  res <- tbl(src = options()$statskier_src,"main") %>%
    filter(date >= cur_date) %>%
    select(raceid,date,location,cat1,cat2,gender,type,tech,length,start) %>%
    arrange(date,location,cat1,cat2,type,tech) %>%
    collect() %>%
    unique() %>%
    as.data.frame()
  res
}

#' Find Races
#'
#' Search for races on a specific date
#'
#' @param date character; e.g. 'YYYY-MM-DD'
#' @return A data frame with information on every race for the given date
#' @export
#' @examples
#' \dontrun{
#' find_races(cur_date = '2015-01-01')
#' }
find_races <- function(cur_date){
  res <- tbl(src = options()$statskier_src,"main") %>%
    filter(date == cur_date) %>%
    select(raceid,date,location,cat1,cat2,gender,type,tech,length,start) %>%
    arrange(date,location,cat1,cat2,type,tech,gender) %>%
    collect() %>%
    unique() %>%
    as.data.frame()
  res
}

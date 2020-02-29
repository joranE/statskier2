#' Recent Races
#'
#' Utility function for listing the most recent
#' events in the database since a given date. Used mainly
#' to help track what new races need to be entered.
#'
#' @param cur_date character; a date in the format "YYYY-MM-DD"
#' @return A data frame with one row for each race listing various details
#' @export
#' @examples
#' \dontrun{
#' x <- recent_events("2016-02-01")
#' head(x)
#' }
recent_events <- function(cur_date){
  res <- tbl(src = conl,"v_event") %>%
    filter(date >= cur_date) %>%
    select(eventid,date,location,event_type,cat1,cat2,gender,tech,length,format) %>%
    arrange(date,location) %>%
    collect() %>%
    unique() %>%
    as.data.frame()
  res
}

#' Find Races
#'
#' Search for events on a specific date
#'
#' @param date character; e.g. 'YYYY-MM-DD'
#' @return A data frame with information on every race for the given date
#' @export
#' @examples
#' \dontrun{
#' find_events(cur_date = '2015-01-01')
#' }
find_events <- function(cur_date){
  res <- tbl(conl,"v_event") %>%
    filter(date == cur_date) %>%
    select(eventid,date,location,event_type,cat1,cat2,gender,tech,length,format) %>%
    arrange(date,location) %>%
    collect() %>%
    unique() %>%
    as.data.frame()
  res
}

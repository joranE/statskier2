#' List most recent XC races in db
#'
#' @param cur_date character; a date
#' @export
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

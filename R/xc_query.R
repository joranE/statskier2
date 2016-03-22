#' General query function
#'
#' @param sql_query character; sql query
#' @export
xc_query <- function(sql_query){
  res <- tbl(src = options()$statskier_src,from = sql(sql_query)) %>%
    collect() %>%
    as.data.frame()
  res
}

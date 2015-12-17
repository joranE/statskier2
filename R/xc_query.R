#' General query function
#'
#' @param sql_query character; sql query
#' @export
xc_query <- function(sql_query){
  src <- src_sqlite(path = statskier2:::sqlite_path,create = FALSE)
  res <- tbl(src = src,from = sql(sql_query)) %>%
    collect() %>%
    as.data.frame()
  res
}

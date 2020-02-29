#' Manually Create Database Connections
#'
#' Use these functions to manually create connections to
#' either a local SQLite database or a remote MySQL database.
#'
#' They look for connection information in `options("sqlite_path")` and
#' `options("mysql")`.
#'
#' These connections can then be passed to \code{\link{ss_query}}
#' when performing arbitrary SQL queries on the data.
#' @return A database connection object
#' @export
#' @import RSQLite
#' @examples
#' \dontrun{
#' conl <- db_xc_local()
#' ss_query(conl,"select * from main where name = 'RANDALL Kikkan' limit 3")
#' }
db_xc_local <- function(){
  dbConnect(RSQLite::SQLite(), options()$sqlite_path)
}

#' @rdname db_xc_local
#' @export
#' @import RMySQL
db_xc_remote <- function(){
  dbConnect(drv = RMySQL::MySQL(),
            dbname = options()$mysql$dbName,
            username = options()$mysql$user,
            password = options()$mysql$password,
            host = options()$mysql$host,
            port = options()$mysql$port)
}

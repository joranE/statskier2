#' Manually Create Database Connections
#'
#' Use these functions to manually create connections to
#' either a local SQLite database or a remote MySQL database.
#'
#' These connections should then be passed to \code{\link{ss_query}}
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

#' Query XC Results Database
#'
#' @param con database connection as generated via \code{\link{db_xc_local}} or
#' \code{\link{db_xc_remote}}
#' @param q character; SQL
#'
#' @import DBI
#' @export
#' @examples
#' \dontrun{
#' conl <- db_xc_local()
#' ss_query(conl,"select * from main where name = 'RANDALL Kikkan' limit 3")
#' }
ss_query <- function(con,q){
  dbGetQuery(con,q)
}

#' @export
ss_query_ <- function(sql,con){
  dbGetQuery(con,sql)
}

choose_src <- function(){
  if (!is.null(options()$sqlite_path) && file.exists(options()$sqlite_path)){
    src <- src_sqlite(path = options()$sqlite_path,create = FALSE)
  }else{
    if (!is.null(options()$mysql)){
      src <- src_mysql(dbname = options()$mysql$dbName,
                       host = options()$mysql$host,
                       port = options()$mysql$port,
                       user = options()$mysql$user,
                       password = options()$mysql$password)
    }else{
      warning("Neither sqlite_path nor mysql are set in options().",call. = FALSE)
      return(NULL)
    }
  }
  src
}


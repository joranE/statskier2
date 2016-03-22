#' Create Local XC DB Connection
#'
#' @export
#' @import RSQLite
db_xc_local <- function(){
  dbConnect(SQLite(), options()$sqlite_path)
}

#' Create Remote XC DB Connection
#'
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

#' Query XC Database
#'
#' @param con database connection
#' @param q character; SQL
#'
#' @import DBI
#' @export
ss_query <- function(con,q){
  dbGetQuery(con,q)
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


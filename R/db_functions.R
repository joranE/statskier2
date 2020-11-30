#' Create Connection to StatSkier DB
#'
#' Call this function with no arguments to create a hidden object in the
#' global environment that acts as the db connection. This function expects
#' all the connection information to be stored in `options()$statskier_pg` as
#' a list with elements host, dbname, user, password, port and sslmode.
#'
#' The hidden db connection object is called \code{..statskier_pg_con..}.
#'
#' @return A database connection object
#' @export
#' @import RPostgres
statskier_connect <- function(){
  con_info <- options()$statskier_pg
  if (!is.null(con_info)){
    con <- RPostgres::dbConnect(RPostgres::Postgres(),
                                host = con_info$host,
                                dbname = con_info$dbname,
                                user = con_info$user,
                                password = con_info$password,
                                port = con_info$port,
                                sslmode = con_info$sslmode)
    assign(x = "..statskier_pg_con..",value = con,envir = .GlobalEnv)
    message("Connected to statskier db.")
  }else{
    message("Connection info not found in options('statskier_pg').")
  }
}

#' @export
statskier_disconnect <- function(){
  if (!exists(x = "..statskier_pg_con..",envir = .GlobalEnv)){
    stop("Connection object ..statskier_pg_con.. not found.")
  } else {
    RPostgres::dbDisconnect(..statskier_pg_con..)
    message("Disonnected from statskier db.")
  }
}

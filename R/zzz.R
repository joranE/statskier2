#' @export
`%ni%` <- Negate(`%in%`)

WC_POINTS <- c(100,80,60,50,45,40,36,32,29,26,24,22,20,18,16:1)
WC_POINTS_STAGE <- c(50,46,43,40,37,34,32,30,28,26,24,22,20,18,16,15:1)
MAJ_INT <- c('WC','OWG','WSC','TDS')

#' @export
.db <- "~/Dropbox/SkiingResults"

#' @export
.dbm <- "~/Dropbox/SkiingResults/misc"

sqlite_path <- "~/Dropbox/UM/Packages/db/fis_new.db"

# .onLoad <- function(libname,pkgname){
#   #Current options
#   op <- options()
#
#   #Attempt connections
#   con_remote <- tryCatch({RMySQL::dbConnect(drv = RMySQL::MySQL(),
#                                             dbname = options()$mysql$dbName,
#                                             username = options()$mysql$user,
#                                             password = options()$mysql$password,
#                                             host = options()$mysql$host,
#                                             port = options()$mysql$port)},
#                          error = function(e) e)
#   con_local <- tryCatch({RSQLite::dbConnect(RSQLite::SQLite(),
#                                             sqlite_path)},
#                         error = function(e) e)
#
#   if (!inherits(con_remote,"MySQLConnection")){
#     cat(con_remote$message)
#     con_remote <- NULL
#   }
#   if(!inherits(con_local,"SQLiteConnection")){
#     cat(con_local$message)
#     con_local <- NULL
#   }
#   op.statskier <- list(
#     statskier_remote = con_remote,
#     statskier_local = con_local
#   )
#   #toset <- !(names(op.statskier) %in% names(op))
#   options(op.statskier)
#   invisible()
# }

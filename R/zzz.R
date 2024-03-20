#' @export
`%ni%` <- Negate(`%in%`)

WC_POINTS <- c(100, 80, 60, 50, 45, 40, 36, 32, 29, 26, 24, 22, 20, 18, 16:1)
WC_POINTS_STAGE <- c(50, 46, 43, 40, 37, 34, 32, 30, 28, 26, 24, 22, 20, 18, 16, 15:1)
MAJ_INT <- c("wc", "owg", "wsc", "tds")

#' @export
.db <- "~/Dropbox/SkiingResults"

#' @export
.dbm <- "~/Dropbox/SkiingResults/misc"

# conl <- RSQLite::dbConnect(RSQLite::SQLite(),
#                            dbname = "~/Dropbox/new-results-db/output/fis_results_prototype.db")

# .onDetach <- function(libpath){
#   RSQLite::dbDisconnect(conl)
# }

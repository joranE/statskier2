#' @export
`%ni%` <- Negate(`%in%`)

WC_POINTS <- c(100,80,60,50,45,40,36,32,29,26,24,22,20,18,16:1)
WC_POINTS_STAGE <- c(50,46,43,40,37,34,32,30,28,26,24,22,20,18,16,15:1)
MAJ_INT <- c('WC','OWG','WSC','TDS')

#' @export
.db <- "~/Dropbox/SkiingResults"

#' @export
.dbm <- "~/Dropbox/SkiingResults/misc"

.onAttach <- function(libname,pkgname){
  src <- choose_src()
  options(statskier_src = src)
  invisible()
}

.onDetach <- function(libpath){
  if (!is.null(options()$statskier_src) && inherits(options()$statskier_src,"src_mysql")){
    DBI::dbDisconnect(options()$statskier_src$con)
  }
  invisible()
}

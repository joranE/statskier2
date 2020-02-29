#' Open Race URL in Browser
#'
#' Open results for a race in a web browser.
#'
#' @param raceid integer
#' @export
#' @family race info functions
#' @examples
#' \dontrun{
#' open_race_url(raceid = 7902)
#' }
open_race_url <- function(raceid){
  if (length(raceid) > 1){
    raceid <- raceid[1]
    warning("Ignoring all but first raceid...",immediate. = TRUE)
  }
  rid <- raceid
  urls <- tbl(options()$statskier_src,"race_url") %>%
    filter(raceid == rid) %>%
    collect()
  if (nrow(urls) == 0){
    stop("No URLs for that raceid yet.")
  }
  url1 <- urls$url1
  url2 <- urls$url2

  if (!is.na(url1)){
    browseURL(url = url1)
  }
  if(!is.na(url2)){
    browseURL(url = url2)
  }
}

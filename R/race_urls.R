#' Open Race URL in Browser
#'
#' Open results for a race in a web browser.
#'
#' @param eventid integer
#' @export
#' @family race info functions
#' @examples
#' \dontrun{
#' open_race_url(eventid = 7902)
#' }
open_race_url <- function(eventid){
  if (length(raceid) > 1){
    raceid <- raceid[1]
    warning("Ignoring all but first eventid...",immediate. = TRUE)
  }
  eid <- eventid
  urls <- tbl(src = ..statskier_pg_con..,
              dbplyr::in_schema("public","v_event_url")) |>
    filter(eventid == eid) |>
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

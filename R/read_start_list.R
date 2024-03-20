#' Scrape Race Start Lists
#'
#' Simple function to grab start lists from FIS website.
#'
#' @import rvest
#' @export
read_start_list <- function(url, type = c("sprint", "distance")) {
  row_text_extractor <- function(x){
    cl <- rvest::html_attr(x,"class")
    if (!cl %in% c("g-row justify-sb","g-xs-24 bold","g-xs-24 container","container g-xs-24")){
      browser()
      stop("Encountered unknown row class in html:",cl)
    }
    if (cl %in% c("g-row justify-sb")){
      out <- x %>%
        html_children() %>%
        html_text() %>%
        stringr::str_trim()
    }
    if (cl %in% c("g-xs-24 bold","g-xs-24 container","container g-xs-24")){
      out <- x %>%
        html_text() %>%
        stringr::str_trim()
    }
    out
  }
  page <- read_html(x = url)

  page_tbl <- page %>%
    html_nodes(css = ".g-row.justify-sb,.g-xs-24.bold,.g-xs-24.container") %>%
    purrr::map(.f = row_text_extractor)

  first_row <- min(which(sapply(page_tbl,function(x) x[1] %in% c("Order","Rank"))))
  page_tbl <- page_tbl[first_row:length(page_tbl)]
  cn <- page_tbl[[1]]
  page_tbl <- page_tbl[-1]

  start_list <- page_tbl |>
    purrr::map(.f = function(x) setNames(x,cn[1:length(x)]))
  start_list <- bind_rows(!!!start_list)
  start_list
}

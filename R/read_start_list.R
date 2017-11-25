#' @import rvest
#' @export
read_start_list <- function(url,type = c('sprint','distance')){
  tbl <- read_html(x = url) %>%
    html_table(fill = TRUE)

  tbl <- tbl[[2]][,1:3] %>%
    rename(fisid = `FIS Code`,
           name = Name,
           bib = Bib) %>%
    select(fisid,name,bib) %>%
    mutate(fisid = as.character(fisid))

  tbl
}

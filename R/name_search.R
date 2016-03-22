#' @export
find_skier <- function(x){
  x <- paste0("%",x,"%")
  results <- tbl(src = options()$statskier_src,"main") %>%
    select(fisid,compid,name,gender,nation) %>%
    filter(name %like% x) %>%
    collect() %>%
    unique()
  results
}

#' Extract All Cap Words
#'
#' @param x character vector
#' @importFrom stringr str_extract str_trim
#' @export
extract_all_caps <- function(x){
  stringr::str_extract(string = x,pattern = "([A-Z-]+[:space:])+") %>%
    stringr::str_trim()
}

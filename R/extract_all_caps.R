#' Extract All Cap Words
#'
#' @param x character vector
#' @importFrom stringr str_extract str_trim
#' @export
extract_all_caps <- function(x){
  stringr::str_extract(string = x,pattern = "([A-Z-]+[:space:])+") %>%
    stringr::str_trim()
}

#' Shorten Skier Names
#'
#' Grabs the all caps portion and applies title case
#'
#' @param x character vector of skier names
#' @importFrom stringr str_to_title
#' @export
shorten_names <- function(x){
  x %>%
    stringr::str_extract(string = .,pattern = "^[A-Z]*") %>%
    stringr::str_to_title()
}

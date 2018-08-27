#' Extract All Cap Words
#'
#' Helper function to extract only the portion of a string that is in all caps.
#' Useful for athlete name parsing since FIS displays names as: LAST_NAME First_Name.
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
#' Grabs the all caps portion or a string and applies title case
#'
#' @param x character vector of skier names
#' @importFrom stringr str_to_title
#' @importFrom purrr map
#' @export
shorten_names <- function(x){
  x %>%
    stringr::str_extract_all(string = .,pattern = "\\b[A-Z]+\\b") %>%
    purrr::map(.f = paste,collapse = " ") %>%
    stringr::str_to_title()
}

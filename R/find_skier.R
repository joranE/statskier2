#' Simple Fuzzy Searching By Partial Name
#'
#' Since it is (mostly) easier to remember people's names than their
#' id numbers, this is a very simple function for looking up the "right"
#' spelling for an athlete's name.
#'
#' Simply looks for names matching "\%string\%".
#'
#' @param x character; a string you think should be in someone's name
#' @return A data frame with unique individuals who's name contains
#' the given string
#' @export
#' @examples
#' \dontrun{
#' find_skier("OSGOOD")
#' find_skier("Bente")
#' }
find_skier <- function(x){
  skier <- dplyr::tbl(src = ..statskier_pg_con..,
                      dbplyr::in_schema("public","skier"))

  x <- paste0("%",x,"%")
  results <- skier |>
    filter(name %like% x) |>
    collect() |>
    mutate_if(.predicate = bit64::is.integer64,.funs = as.integer)
  results
}

#' @export
skier_info <- function(.compid){
  skier <- dplyr::tbl(src = ..statskier_pg_con..,
                      dbplyr::in_schema("public","skier"))

  result <- skier |>
    filter(compid %in% .compid) |>
    collect() |>
    mutate_if(.predicate = bit64::is.integer64,.funs = as.integer)

  result
}

#' Recent Races
#'
#' Utility function for listing the most recent
#' events in the database since a given date. Used mainly
#' to help track what new races need to be entered.
#'
#' @param cur_date character; a date in the format "YYYY-MM-DD"
#' @return A data frame with one row for each race listing various details
#' @export
#' @import dplyr
#' @examples
#' \dontrun{
#' x <- recent_events("2016-02-01")
#' head(x)
#' }
recent_events <- function(cur_date = as.character(Sys.Date() - 14)) {
  res <- dplyr::tbl(
    src = ..statskier_pg_con..,
    dbplyr::in_schema("public", "v_event")
  ) |>
    filter(date >= cur_date) |>
    select(eventid, date, location, site, event_type, primary_tag, gender, tech, length, format) |>
    arrange(date, location) |>
    collect() |>
    mutate_if(.predicate = bit64::is.integer64, .funs = as.integer) |>
    unique() |>
    as.data.frame()
  res
}

#' Find Races
#'
#' Search for events on a specific date
#'
#' @param date character; e.g. 'YYYY-MM-DD'
#' @return A data frame with information on every race for the given date
#' @export
#' @examples
#' \dontrun{
#' find_events(cur_date = "2015-01-01")
#' }
find_events <- function(cur_date) {
  res <- dplyr::tbl(src = ..statskier_pg_con.., dbplyr::in_schema("public", "v_event")) |>
    filter(date == cur_date) |>
    select(eventid, date, location, site, event_type, primary_tag, gender, tech, length, format) |>
    arrange(date, location) |>
    collect() |>
    mutate_if(.predicate = bit64::is.integer64, .funs = as.integer) |>
    unique() |>
    as.data.frame()
  res
}

#' @export
get_event <- function(.eventid = NULL) {
  tbl_all_ev <- dplyr::tbl(
    src = ..statskier_pg_con..,
    dbplyr::in_schema("public", "all_event")
  )
  ev_type <- tbl_all_ev |>
    filter(eventid == .eventid) |>
    collect()

  if (nrow(ev_type) == 0) {
    stop(sprintf("No event with eventid %s exists.", .eventid))
  }

  tbl_type <- switch(ev_type$event_type,
    "Sprint" = "v_sprint",
    "Distance" = "v_distance",
    "Stage" = "v_stage"
  )

  v_res <- dplyr::tbl(src = ..statskier_pg_con.., dbplyr::in_schema("public", tbl_type))
  ev_results <- v_res |>
    filter(eventid == .eventid) |>
    collect() |>
    mutate_if(.predicate = bit64::is.integer64, .funs = as.integer)

  ev_results
}

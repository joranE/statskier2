#' Generate ELO Distance Input Data
#'
#' Organizes race data into a list by race event that can then be passed to
#' the functions `skierELO::calc_elo_dst()` or `skierELO::calc_elo_spr()` to
#' generate ELO ratings.
#'
#' @param default_rating skiers default rating
#' @param maj_int boolean; restrict to only major international races
#' @export
dst_elo_data <- function(default_rating = 1300,maj_int = FALSE){

  dst <- tbl(src = options()$statskier_src,"main") %>%
    filter(type == 'Distance') %>%
    arrange(date,raceid,rank) %>%
    collect()

  if (maj_int){
    dst <- dst %>%
      filter(cat1 %in% MAJ_INT)
  }

  current_rating_dst <- dst %>%
    select(gender,fisid,name,nation,date) %>%
    unique() %>%
    group_by(gender,fisid,name) %>%
    summarise(nation = nation[date == max(date)][1])
  current_rating_dst$race_count <- 0L
  current_rating_dst$cur_rating <- default_rating

  raceid_order <- unique(dst$raceid)
  dst <- split(dst,dst$raceid)
  dst <- dst[as.character(raceid_order)]

  return(list(races = dst,
              init_rating = current_rating_dst))
}

#' Generate ELO Sprint Input Data
#'
#' Organizes race data into a list by race event that can then be passed to
#' the functions `skierELO::calc_elo_dst()` or `skierELO::calc_elo_spr()` to
#' generate ELO ratings.
#'
#'
#' @param default_rating skiers default rating
#' @param maj_int boolean; restrict to only major international races
#' @export
spr_elo_data <- function(default_rating = 1300,maj_int = FALSE){

  spr <- tbl(src = options()$statskier_src,"main") %>%
    filter(type == 'Sprint') %>%
    arrange(date,raceid,rank) %>%
    collect()

  if (maj_int){
    spr <- spr %>%
      filter(cat1 %in% MAJ_INT)
  }

  current_rating_spr <- spr %>%
    select(gender,fisid,name,nation,date) %>%
    unique() %>%
    group_by(gender,fisid,name) %>%
    summarise(nation = nation[date == max(date)][1])
  current_rating_spr$race_count <- 0L
  current_rating_spr$cur_rating <- default_rating

  raceid_order <- unique(spr$raceid)
  spr <- split(spr,spr$raceid)
  spr <- spr[as.character(raceid_order)]

  return(list(races = spr,
              init_rating = current_rating_spr))
}

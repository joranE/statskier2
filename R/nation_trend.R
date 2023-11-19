#' Performance Trends By Nation
#'
#' Only considers results from major international races and
#' summarises them by the number of race results at a particular
#' level (wins, podiums, etc.) per race.
#'
#' @param .nation character vector of nation codes
#' @return A named list with components:
#' \enumerate{
#'  \item \code{plot} - ggplot2 plot object
#'  \item \code{nation_data} - raw data for each nation
#'  \item \code{nation_summary} - summarised data by nation
#'  \item \code{race_data} - race counts used for plotting
#' }
#' @importFrom tidyr gather
#' @importFrom zoo rollmeanr
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- nation_trend(nations = c('USA','CAN','RUS','SWE'),
#'                   .gender = 'Men',
#'                   .type = 'Distance')
#' print(p$plot)
#' }
nation_dst_trend <- function(.nation,
                             levels = c('Wins' = 1,'Podiums' = 3,'Top10' = 10,'Top30' = 30)){
  v_dst <- dplyr::tbl(src = ..statskier_pg_con..,
                      dbplyr::in_schema("public","v_distance_maj_int"))

  if (length(levels) > 4){
    stop("'levels' has a maximum length of 4.")
  }

  mx_lev <- max(levels,na.rm = TRUE)
  dat <- v_dst |>
    filter(nation %in% .nation &
             !is.na(rank) &
             rank <= mx_lev &
             nation != location) |>
    collect()

  dat <- dat |>
    mutate(rank_cat = cut(x = rank,breaks = c(0,levels),labels = names(levels))) |>
    group_by(nation,gender,season) |>
    mutate(n_ev = n_distinct(eventid))

  dat_grp <- dat |>
    group_by(nation,gender,season,rank_cat) |>
    summarise(n = n(),
              pct = n / n_ev[1]) |>
    ungroup() |>
    mutate(date = season_to_date(season)) |>
    group_by(nation,gender,rank_cat) |>
    arrange(date) |>
    mutate(pct_roll = zoo::rollmeanr(x = pct,k = 3,fill = NA))

  p <- ggplot(data = dat_grp,aes(x = as.Date(date),y = pct_roll,color = rank_cat,group = rank_cat)) +
    facet_grid(gender~nation) +
    geom_line() +
    labs(x = "Date",y = NULL,color = "Level") +
    scale_color_brewer(palette = "Set2") +
    theme_bw()
  list(p,dat_grp)
}


#' @export
nation_spr_trend <- function(.nation,
                             levels = c('Wins' = 1,'Finals' = 6,'Semis' = 12,'Quals' = 30)){
  v_spr <- dplyr::tbl(src = ..statskier_pg_con..,
                      dbplyr::in_schema("public","v_sprint_maj_int"))

  if (length(levels) > 4){
    stop("'levels' has a maximum length of 4.")
  }

  mx_lev <- max(levels,na.rm = TRUE)
  dat <- v_spr |>
    filter(nation %in% .nation &
             !is.na(rank) &
             rank <= mx_lev &
             nation != location) |>
    collect()

  dat <- dat |>
    mutate(rank_cat = cut(x = rank,
                          breaks = c(0,levels),
                          labels = names(levels))) |>
    group_by(nation,gender,season) |>
    mutate(n_ev = n_distinct(eventid))

  dat_grp <- dat |>
    group_by(nation,gender,season,rank_cat) |>
    summarise(n = n(),
              pct = n / n_ev[1]) |>
    ungroup() |>
    mutate(date = season_to_date(season)) |>
    group_by(nation,gender,rank_cat) |>
    arrange(date) |>
    mutate(pct_roll = zoo::rollmeanr(x = pct,k = 3,fill = NA))

  p <- ggplot(data = dat_grp,aes(x = as.Date(date),
                                 y = pct_roll,
                                 color = rank_cat,
                                 group = rank_cat)) +
    facet_grid(gender~nation) +
    geom_line() +
    labs(x = "Date",y = NULL,color = "Level") +
    scale_color_brewer(palette = "Set2") +
    theme_bw()
  list(p,dat_grp)
}

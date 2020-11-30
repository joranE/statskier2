#' Head-to-Head Against Skiers in a Specific Race
#'
#' Compare results of \code{ath1_name} in a specific race (\code{race_id})
#' to the historical record of how \code{ath1_name} has performed against the
#' particular opponents in that race. Serves as a slightly more "objective" way
#' to judge whether \code{ath1_name} had a good, bad or average race.
#'
#' @param ath1_name character
#' @param race_id integer
#' @param num_opp integer; limit comparison to opponents in the top \code{num_opp}.
#' Defaults to \code{Inf} meaning look at all opponents.
#' @param cutoff integer; length of time in days to look at historical results
#' @param min_encounters integer; minimum number of time \code{ath1_name} must have
#' faced an opponent for the opponent to be included in the comparison
#' @param measure character; one of \code{"rank"}, \code{"fispoints"} or \code{"pb"}
#' @param restrict_by currently unused
#' @export
#' @examples
#' \dontrun{
#' x <- hth_race('DIGGINS Jessica',9232,num_opp = 30,min_encounters = 3,measure = 'pb')
#' }
hth_race <- function(ath_names,
                     race_id,
                     num_opp = Inf,
                     cutoff = 365 * 5,
                     min_encounters = 1,
                     measure = c('rank','fispoints','pb'),
                     events = c('all','maj_int'),
                     restrict_by = NULL){

  measure <- match.arg(measure)
  events = match.arg(events)

  race_data <- tbl(src = ..statskier_pg_con..,
                   dbplyr::in_schema("public","main")) %>%
    filter(raceid == race_id) %>%
    collect() %>%
    filter(rank <= num_opp | name %in% ath_names)

  opp_names <- race_data %>%
    filter(!name %in% ath_names) %>%
    pull(var = "name")

  race_info <- race_data %>%
    select(raceid,date,type,tech,start,length) %>%
    distinct()

  race_day <- as.integer(as.Date(race_info$date))

  hth_df <- hth_data(athletes = ath_names,
                     opponents = opp_names) %>%
    filter(n_races >= min_encounters &
             race_day - as.integer(as.Date(date)) <= cutoff &
             type == race_info$type)

  if (events == 'maj_int'){
    hth_df <- filter(hth_df,cat1 %in% c('WC','TDS','OWG','WSC'))
  }

  hth_df$y <- hth_df[[paste0("diff_",measure)]]
  ylab <- switch(measure,
                 'rank' = 'Finishing Place',
                 'fispoints' = 'FIS Points',
                 'pb' = 'Percent Back')

  #Extract computed data frame from violin plot
  tmp_plot <- hth_df %>%
    filter(raceid != race_id) %>%
    ggplot(data = .,aes(x = factor(season),y = y)) +
      facet_wrap(~ath_name) +
      geom_violin()
  tmp_plot_data <- ggplot2::ggplot_build(tmp_plot)$data[[1]] %>%
    mutate(facet_var = ath_names[PANEL])

  #Adjust to make suitable for geom_polygon
  tmp_plot_data <- tmp_plot_data %>%
    mutate(xminv = x - violinwidth * (x - xmin),
           xmaxv = x + violinwidth * (xmax - x))
  copy1 <- tmp_plot_data %>%
    mutate(x = xminv) %>%
    arrange(y)
  copy2 <- tmp_plot_data %>%
    mutate(x = xmaxv) %>%
    arrange(-y)
  polygon_data <- bind_rows(copy1,copy2) %>%
    mutate(group1 = interaction(factor(group),factor(y >= 0)),
           fill_group = if_else(y >= 0,'Athlete Wins','Opponent Wins'))

  cur_race <- hth_df %>%
    mutate(season = as.integer(factor(season)),
           facet_var = ath_name) %>%
    filter(raceid == race_id)

  x_labs <- factor(unique(hth_df$season))


  p <- ggplot() +
    facet_wrap(~facet_var) +
    geom_polygon(data = polygon_data,
                 aes(x = x,y = y,
                     group = group1,fill = fill_group),
                 alpha = 0.25) +
    geom_violin(data = cur_race,
                aes(x = season,y = y),
                fill = "grey20",
                alpha = 0.25) +
    scale_x_continuous(breaks = as.integer(x_labs),
                       labels = levels(x_labs)) +
    labs(x = NULL,y = ylab,fill = "") +
    theme(axis.text.x = element_text(angle = 310,
                                     hjust = 0,
                                     vjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal")


  return(list(plot = p,
              race_data = race_data,
              hth_df = hth_df))
}

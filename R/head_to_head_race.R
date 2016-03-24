#' @export
#' @examples
#' \dontrun{
#' x <- hth_race('DIGGINS Jessica',9232,num_ath2 = 30,min_encounters = 3,measure = 'pb')
#' }
hth_race <- function(ath1_name,
                     race_id,
                     num_ath2 = Inf,
                     cutoff = 365 * 5,
                     min_encounters = 1,
                     measure = c('rank','fispoints','pb'),
                     restrict_by = NULL){

  measure <- match.arg(measure)

  race_data <- tbl(src = options()$statskier_src,"main") %>%
    filter(raceid == race_id) %>%
    collect() %>%
    filter(rank <= num_ath2)

  ath2_names <- select(race_data,fisid,name) %>%
    filter(name != ath1_name)

  race_info <- race_data %>%
    select(raceid,type,tech,start,length) %>%
    unique()

  if (race_info$type == 'Distance'){
    hth_data <- hth_dst(ath1 = ath1_name,
                        ath2 = ath2_names$name,
                        races = "fis",
                        measure = measure)$data %>%
      group_by(facet_name) %>%
      mutate(n_races = n_distinct(raceid)) %>%
      ungroup() %>%
      filter(n_races >= min_encounters) %>%
      as.data.frame()
  }
  if(race_info$type == 'Sprint'){
    hth_data <- hth_spr(ath1 = ath1_name,
                        ath2 = ath2_names,
                        races = "fis",
                        measure = measure)$data
  }

  hth_data$y <- hth_data[[paste0("d",measure)]]
  ylab <- switch(measure,
                 'rank' = 'Finishing Place',
                 'fispoints' = 'FIS Points',
                 'pb' = 'Percent Back')
  hth_data$victor <- ifelse(hth_data$y >= 0,
                            paste(extract_all_caps(ath1_name),"Wins"),
                            'Opponent Wins')

  #Extract computed data frame from violin plot
  tmp_plot <- ggplot() +
    geom_violin(data = hth_data[hth_data$raceid != race_id,],
                aes(x = factor(season),y = y))
  tmp_plot_data <- ggplot2::ggplot_build(tmp_plot)$data[[1]]

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
           fill_group = ifelse(y >= 0,paste(extract_all_caps(ath1_name),'Wins'),'Opponent Wins'))

  cur_race <- hth_data %>%
    mutate(season = factor(season)) %>%
    filter(raceid == race_id) %>%
    mutate(season = as.integer(season))

  x_labs <- factor(unique(hth_data$season))


  p <- ggplot() +
        geom_polygon(data = polygon_data,
                     aes(x = x,y = y,group = group1,fill = fill_group),
                     alpha = 0.25) +
    geom_violin(data = cur_race,
                aes(x = season,y = y),
                fill = "grey20",
                alpha = 0.25) +
    scale_x_continuous(breaks = as.integer(x_labs),
                       labels = levels(x_labs)) +
    labs(x = NULL,y = ylab,fill = "") +
    theme(legend.position = "bottom",
          legend.direction = "horizontal")


  return(list(plot = p,
              race_data = race_data,
              hth_data = hth_data))
}

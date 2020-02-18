#' Distance Split Times Plot
#'
#' @importFrom egg ggarrange
#' @export
dst_split_plot <- function(data,n_skiers = 30,n_seg = 6){
  conl <- db_xc_local()
  maj_int <- tbl(conl,"maj_int")
  race_info <- maj_int %>%
    filter(raceid == !!data$raceid[1]) %>%
    select(raceid,date,season,location,tech,length,gender,start) %>%
    distinct() %>%
    collect()

  # if (race_info$start == "Handicap"){
  #   warning("This plot may not make much sense for a handicap start.",immediate. = TRUE)
  # }
  title <- paste(race_info$date,race_info$location,race_info$gender,
                 paste0(race_info$length,"km"),race_info$tech,race_info$start)

  dst_race <- data %>%
    filter(!is.na(split_km)) %>%
    group_by(split_km) %>%
    mutate(time_back = split_time - min(split_time,na.rm = TRUE),
           pct_back = time_back / min(split_time,na.rm = TRUE),
           name = stringr::str_trim(name,side = "both")) %>%
    ungroup()

  if (race_info$start == "Handicap"){
    dst_race <- dst_race %>%
      filter(split_km != min(split_km,na.rm = TRUE))
  }

  top_end <- dst_race %>%
    filter(split_km == max(split_km,na.rm = TRUE) &
             split_rank <= n_skiers) %>%
    mutate(name2 = extract_all_caps(name))
  top_begin <- dst_race %>%
    filter(split_km == min(split_km,na.rm = TRUE) &
             name %in% top_end$name) %>%
    mutate(name2 = extract_all_caps(name))
  top_seg <- dst_race %>%
    group_by(name) %>%
    arrange(split_km) %>%
    mutate(seg_time = c(split_time[1],diff(split_time))) %>%
    group_by(split_km) %>%
    mutate(seg_rank = min_rank(seg_time)) %>%
    as.data.frame() %>%
    filter(seg_rank <= n_seg)

  subt <- "Only shows top %s finishers.\nRed numbers indicate %s fastest splits over that section (may not be a person in the top %s)."
  subt <- sprintf(subt,n_skiers,n_seg,n_skiers)

  p_top <- dst_race %>%
    filter(name %in% top_end$name) %>%
    mutate(facet_label = "Percent Back by Split") %>%
    ggplot(data = .,aes(x = split_km,y = pct_back,group = name)) +
    facet_wrap(~facet_label) +
    geom_line(alpha = 0.5) +
    geom_text(data = top_begin,aes(label = name2),hjust = 1.1,size = 2,color = "blue") +
    geom_text(data = top_end,aes(label = name2),hjust = -0.1,size = 2,color = "blue") +
    geom_text(data = top_seg,aes(label = seg_rank),size = 2.5,color = "red") +
    scale_x_continuous(breaks = unique(dst_race$split_km),expand = expand_scale(mult = 0.1)) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = NULL,y = "Percent Back") +
    ggtitle(label = title,
            subtitle = subt) +
    theme_bw()

  p_bottom <- dst_race %>%
    filter(name %in% top_end$name) %>%
    mutate(facet_label = "Race Position by Split") %>%
    ggplot(data = .,aes(x = split_km,y = split_rank,group = name)) +
    facet_wrap(~facet_label) +
    geom_line(alpha = 0.5) +
    geom_text(data = top_begin,aes(label = name2),hjust = 1.1,size = 2,color = "blue") +
    geom_text(data = top_end,aes(label = name2),hjust = -0.1,size = 2,color = "blue") +
    geom_text(data = top_seg,aes(label = seg_rank),size = 2.5,color = "red") +
    scale_x_continuous(breaks = unique(dst_race$split_km),expand = expand_scale(mult = 0.1)) +
    labs(x = "Km",y = "Position",caption = "@statskier - statisticalskier.com") +
    theme_bw()

  final_plot <- egg::ggarrange(p_top,p_bottom,nrow = 2,draw = FALSE)
  return(final_plot)
}

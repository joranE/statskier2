#' Race Snapshots
#'
#' Distance and sprint race snapshot graphs for major international
#' distance events.
#'
#' @param race_id integer race id
#' @param title race title
#' @param cutoff integer number of days in past to gether data from
#' @param reduced boolean; if \code{TRUE} only show top 30 plus North Americans
#' @return A named list with components:
#' \enumerate{
#'  \item \code{plot} - ggplot2 plot object
#'  \item \code{cur_race} - raw results for race in question
#'  \item \code{ath_min} - data for athletes with too few races to plot range bars
#'  \item \code{ath_bars} - data for athlete range bars
#'  \item \code{block} - data frame for color blocks in plot
#' }
#' @import ggplot2
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- race_snapshot_dst(race_id = 7902)
#' print(p$plot)
#' }
race_snapshot_dst <- function(race_id,
                              title = "",
                              cutoff = 365 * 4,
                              reduced = TRUE){

  cur_race <- tbl(src = options()$statskier_src,"main") %>%
    filter(raceid == race_id) %>%
    arrange(rank) %>%
    collect() %>%
    mpb() %>%
    standardize_mpb() %>%
    mutate(name1 = paste(shorten_names(name),rank)) %>%
    arrange(rank)

  if (reduced){
    cur_race <- filter(cur_race,rank <= 30 | nation %in% c('USA','CAN'))
  }

  race_date <- cur_race$date[1]
  race_tech <- cur_race$tech[1]
  race_start <- switch(cur_race$start[1],
                       'Interval' = 'Interval',
                       'Mass' = c('Mass','Pursuit'),
                       'Pursuit' = c('Mass','Pursuit'),
                       'Handicap' = 'Handicap',
                       'Pursuit Break' = 'Pursuit Break')
  start_label <- switch(cur_race$start[1],
                        'Interval' = 'Interval',
                        'Mass' = 'Mass',
                        'Pursuit' = 'Mass',
                        'Handicap' = 'Handicap',
                        'Pursuit Break' = 'Pursuit Break')
  tech_label <- switch(race_tech,
                       'F' = 'Freestyle',
                       'C' = 'Classic',
                       'FC' = 'Skiathlon')

  cutoff_date <- as.character(as.Date(race_date) - cutoff)

  race_history <- tbl(src = options()$statskier_src,"main") %>%
    filter(cat1 %in% MAJ_INT &
             name %in% local(cur_race$name) &
             type == 'Distance' &
             date < race_date &
             date >= cutoff_date) %>%
    collect() %>%
    mpb() %>%
    standardize_mpb() %>%
    left_join(cur_race[,c('name','name1')],by = 'name') %>%
    mutate(same_tech = ifelse(tech == race_tech,'Yes','No'),
           same_start = ifelse(start %in% race_start,'Yes','No')) %>%
    group_by(name) %>%
    mutate(nrace_overall = n()) %>%
    group_by(name,same_tech) %>%
    mutate(nrace_tech = n()) %>%
    group_by(name,same_start) %>%
    mutate(nrace_start = n()) %>%
    as.data.frame()

  ath_min_races_overall <- race_history %>%
    filter(nrace_overall < 10) %>%
    as.data.frame()
  ath_min_races_tech <- race_history %>%
    filter(same_tech == 'Yes' & nrace_tech < 10) %>%
    as.data.frame()
  ath_min_races_start <- race_history %>%
    filter(same_start == 'Yes' & nrace_start < 10) %>%
    as.data.frame()
  ath_min <- bind_rows(setNames(list(ath_min_races_overall,ath_min_races_tech,ath_min_races_start),
                                c('Overall',tech_label,start_label)),
                       .id = 'facet_grp')

  ath_bars_overall <- race_history %>%
    filter(nrace_overall >= 10) %>%
    group_by(name1) %>%
    summarise(q25 = quantile(mpb,0.25,na.rm = TRUE),
              q75 = quantile(mpb,0.75,na.rm = TRUE))
  ath_bars_tech <- race_history %>%
    filter(same_tech == 'Yes' & nrace_tech >= 10) %>%
    group_by(name1) %>%
    summarise(q25 = quantile(mpb,0.25,na.rm = TRUE),
              q75 = quantile(mpb,0.75,na.rm = TRUE))
  ath_bars_start <- race_history %>%
    filter(same_start == 'Yes' & nrace_start >= 10) %>%
    group_by(name1) %>%
    summarise(q25 = quantile(mpb,0.25,na.rm = TRUE),
              q75 = quantile(mpb,0.75,na.rm = TRUE))
  ath_bars <- bind_rows(setNames(list(ath_bars_overall,ath_bars_tech,ath_bars_start),
                                 c('Overall',tech_label,start_label)),
                        .id = 'facet_grp')

  #Make block data
  n_race <- nrow(cur_race)
  n_block <- (n_race %/% 10) + ((n_race %% 10) > 0)

  mn_idx <- c(1,1+which(seq_len(n_race) %% 10 == 0))
  mx_idx <- c(which(seq_len(n_race) %% 10 == 0),n_race)

  block <- data.frame(ymn = cur_race$name1[mn_idx[seq_len(n_block)]],
                      ymx = cur_race$name1[mx_idx[seq_len(n_block)]],
                      xmn = rep(-Inf,n_block),
                      xmx = rep(Inf,n_block),
                      block = rep(c('block1','block2'),length.out = n_block))
  block <- bind_rows(setNames(list(block,block,block),
                              c('Overall',tech_label,start_label)),
                     .id = 'facet_grp')

  name_order <- rev(cur_race$name1)
  cur_race <- bind_rows(setNames(list(cur_race,cur_race,cur_race),
                                 c('Overall',tech_label,start_label)),
                        .id = 'facet_grp')

  #Set name order
  cur_race$name1 <- factor(cur_race$name1,levels = name_order)
  ath_min$name1 <- factor(ath_min$name1,levels = name_order)
  ath_bars$name1 <- factor(ath_bars$name1,levels = name_order)
  block$ymn <- factor(block$ymn,levels = name_order)
  block$ymx <- factor(block$ymx,levels = name_order)

  #Set facet order
  cur_race$facet_grp <- factor(cur_race$facet_grp,
                               levels = c(tech_label,start_label,"Overall"))
  ath_min$facet_grp <- factor(ath_min$facet_grp,
                              levels = c(tech_label,start_label,"Overall"))
  ath_bars$facet_grp <- factor(ath_bars$facet_grp,
                               levels = c(tech_label,start_label,"Overall"))
  block$facet_grp <- factor(block$facet_grp,
                            levels = c(tech_label,start_label,"Overall"))

  p <- ggplot() +
    facet_wrap(~facet_grp,nrow = 1,scale = "free_x") +
    geom_blank(data = cur_race,aes(x = mpb,y = name1)) +
    geom_rect(data = block,
              aes(ymin = ymn,ymax = ymx,
                  xmin = -Inf,xmax = Inf,
                  fill = block),alpha = 0.25,show.legend = FALSE) +
    geom_segment(data = ath_bars,aes(x = q25,xend = q75,y = name1,yend = name1)) +
    geom_point(data = cur_race,aes(x = mpb,y = name1),color = "red") +
    geom_point(data = ath_min,aes(x = mpb,y = name1),alpha = 0.5) +
    scale_fill_manual(values = c('#778899','#2F4F4F')) +
    ggtitle(label = paste("Race Snapshot - ",title),
            subtitle = "For >=10 prior races, bars represent 25th-75th percentile of past performance") +
    labs(x = 'Standardized % Behind Median Skier',y = 'Athlete',
         fill = "",caption = "statisticalskier.com - @statskier") +
    theme_bw()

  return(list(plot = p,
              cur_race = cur_race,
              ath_min = ath_min,
              ath_bars = ath_bars,
              block = block))
}

#' @rdname race_snapshot_dst
#' @export
race_snapshot_spr <- function(race_id,
                              title = "",
                              cutoff = 365 * 4,
                              reduced = TRUE){

  cur_race <- tbl(src = options()$statskier_src,"main") %>%
    filter(raceid == race_id) %>%
    arrange(rank) %>%
    collect() %>%
    mutate(name1 = paste(shorten_names(name),rank)) %>%
    arrange(rank)

  if (reduced){
    cur_race <- filter(cur_race,rank <= 30 | nation %in% c('USA','CAN'))
  }

  race_date <- cur_race$date[1]
  race_tech <- cur_race$tech[1]
  tech_label <- switch(race_tech,
                       'F' = 'Freestyle',
                       'C' = 'Classic')

  cutoff_date <- as.character(as.Date(race_date) - cutoff)

  race_history <- tbl(src = options()$statskier_src,"main") %>%
    filter(cat1 %in% MAJ_INT &
             name %in% local(cur_race$name) &
             type == 'Sprint' &
             date < race_date &
             date >= cutoff_date) %>%
    collect() %>%
    left_join(cur_race[,c('name','name1')],by = 'name') %>%
    mutate(same_tech = ifelse(tech == race_tech,'Yes','No')) %>%
    group_by(name) %>%
    mutate(nrace_overall = n()) %>%
    group_by(name,same_tech) %>%
    mutate(nrace_tech = n()) %>%
    as.data.frame()

  ath_min_races_overall <- race_history %>%
    filter(nrace_overall < 10) %>%
    as.data.frame()
  ath_min_races_tech <- race_history %>%
    filter(same_tech == 'Yes' & nrace_tech < 10) %>%
    as.data.frame()
  ath_min <- bind_rows(setNames(list(ath_min_races_overall,ath_min_races_tech),
                                c('Overall',tech_label)),
                       .id = 'facet_grp')

  ath_bars_overall <- race_history %>%
    filter(nrace_overall >= 10) %>%
    group_by(name1) %>%
    summarise(q25 = quantile(rank,0.25,na.rm = TRUE),
              q75 = quantile(rank,0.75,na.rm = TRUE))
  ath_bars_tech <- race_history %>%
    filter(same_tech == 'Yes' & nrace_tech >= 10) %>%
    group_by(name1) %>%
    summarise(q25 = quantile(rank,0.25,na.rm = TRUE),
              q75 = quantile(rank,0.75,na.rm = TRUE))
  ath_bars <- bind_rows(setNames(list(ath_bars_overall,ath_bars_tech),
                                 c('Overall',tech_label)),
                        .id = 'facet_grp')

  #Make block data
  n_race <- nrow(cur_race)
  n_block <- (n_race %/% 10) + ((n_race %% 10) > 0)

  mn_idx <- c(1,1 + which(seq_len(n_race) %% 10 == 0))
  mx_idx <- c(which(seq_len(n_race) %% 10 == 0),n_race)

  block <- data.frame(ymn = cur_race$name1[mn_idx[seq_len(n_block)]],
                      ymx = cur_race$name1[mx_idx[seq_len(n_block)]],
                      xmn = rep(-Inf,n_block),
                      xmx = rep(Inf,n_block),
                      block = rep(c('block1','block2'),length.out = n_block))
  block <- bind_rows(setNames(list(block,block),
                              c('Overall',tech_label)),
                     .id = 'facet_grp')

  name_order <- rev(cur_race$name1)
  cur_race <- bind_rows(setNames(list(cur_race,cur_race),
                                 c('Overall',tech_label)),
                        .id = 'facet_grp')

  #Set name order
  cur_race$name1 <- factor(cur_race$name1,levels = name_order)
  ath_min$name1 <- factor(ath_min$name1,levels = name_order)
  ath_bars$name1 <- factor(ath_bars$name1,levels = name_order)
  block$ymn <- factor(block$ymn,levels = name_order)
  block$ymx <- factor(block$ymx,levels = name_order)

  #Set facet order
  cur_race$facet_grp <- factor(cur_race$facet_grp,
                               levels = c(tech_label,"Overall"))
  ath_min$facet_grp <- factor(ath_min$facet_grp,
                              levels = c(tech_label,"Overall"))
  ath_bars$facet_grp <- factor(ath_bars$facet_grp,
                               levels = c(tech_label,"Overall"))
  block$facet_grp <- factor(block$facet_grp,
                            levels = c(tech_label,"Overall"))

  p <- ggplot() +
    facet_wrap(~facet_grp,nrow = 1,scale = "free_x") +
    geom_blank(data = cur_race,aes(x = rank,y = name1)) +
    geom_rect(data = block,
              aes(ymin = ymn,ymax = ymx,
                  xmin = -Inf,xmax = Inf,
                  fill = block),alpha = 0.25,show.legend = FALSE) +
    geom_segment(data = ath_bars,aes(x = q25,xend = q75,y = name1,yend = name1)) +
    geom_point(data = ath_min,aes(x = rank,y = name1),alpha = 0.5) +
    geom_point(data = cur_race,aes(x = rank,y = name1),color = "red") +
    scale_fill_manual(values = c('#778899','#2F4F4F')) +
    ggtitle(label = paste("Race Snapshot - ",title),
            subtitle = "For >=10 prior races, bars represent 25th-75th percentile of past performance") +
    labs(x = 'Finishing Place',y = 'Athlete',
         fill = "",caption = "statisticalskier.com - @statskier") +
    theme_bw()

  return(list(plot = p,
              cur_race = cur_race,
              ath_min = ath_min,
              ath_bars = ath_bars,
              block = block))
}

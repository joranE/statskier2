#' Head-to-head Distance Plot & Data
#'
#' @param ath1 character; athlete of interest
#' @param ath2 character vector of athletes to compare with
#' @param races character; one of "maj_int" or "fis"
#' @param measure character; one of "fispoints", "rank", "time", "pb" or "mpb".
#' "mpb" only is sensible if \code{races = "maj_int"}.
#' @param by_tech boolean; split trend lines by technique. Only one of
#' \code{by_tech} and \code{by_start} can by \code{TRUE}.
#' @param by_start boolean; split trend lines by race type. Only one of
#' \code{by_tech} and \code{by_start} can by \code{TRUE}.
#' @export
hth_dst <- function(ath1,
                    ath2,
                    races = c("maj_int","fis"),
                    measure = c("fispoints","rank","time","pb","mpb"),
                    by_tech = FALSE,
                    by_start = FALSE){
  src <- src_sqlite(path = statskier2:::sqlite_path,create = FALSE)

  if (ath1 %in% ath2){
    ath2 <- setdiff(ath2,ath1)
    warning("Removing ath1 from ath2 names.")
  }

  races <- match.arg(races)
  measure <- match.arg(measure)

  if (by_tech && by_start){
    stop("Can only group by one of technique or start type.")
  }

  if (races == "maj_int"){
    ath1_data <- tbl(src = src,"main") %>%
      filter(name == ath1 &
               cat1 %in% MAJ_INT &
               type == 'Distance') %>%
      collect() %>%
      mpb() %>%
      standardize_mpb() %>%
      select(raceid,date,season,tech,start,
             fisid,name,rank,time,fispoints,mpb)

    winning_times <- tbl(src = src,"main") %>%
      filter(raceid %in% ath1_data$raceid) %>%
      group_by(raceid) %>%
      summarise(winning_time = min(time)) %>%
      collect()

    ath2_data <- tbl(src = src,"main") %>%
      filter(name %in% ath2 &
               cat1 %in% MAJ_INT &
               type == 'Distance' &
               raceid %in% ath1_data$raceid) %>%
      collect() %>%
      mpb() %>%
      standardize_mpb() %>%
      select(raceid,date,season,tech,start,
             fisid,name,rank,time,fispoints,mpb)

    ath_data <- inner_join(ath1_data,
                           ath2_data,
                           by = c('raceid','date','season','tech','start')) %>%
      left_join(winning_times,by = "raceid") %>%
      mutate(start = ifelse(start == 'Pursuit','Mass',start),
             drank = rank.y - rank.x,
             dtime = time.y - time.x,
             dfispoints = fispoints.y - fispoints.x,
             dmpb = mpb.y - mpb.x,
             dpb = 100 * (time.y - time.x) / winning_time) %>%
      select(raceid,date,season,tech,start,fisid.x,name.x,
             fisid.y,name.y,drank,dtime,dfispoints,dmpb,dpb)
  }
  if (races == "fis"){
    ath1_data <- tbl(src = src,"main") %>%
      filter(name == ath1 &
               type == 'Distance') %>%
      collect() %>%
      select(raceid,date,season,tech,start,
             fisid,name,rank,time,fispoints)

    winning_times <- tbl(src = src,"main") %>%
      filter(raceid %in% ath1_data$raceid) %>%
      group_by(raceid) %>%
      summarise(winning_time = min(time)) %>%
      collect()

    ath2_data <- tbl(src = src,"main") %>%
      filter(name %in% ath2 &
               type == 'Distance' &
               raceid %in% ath1_data$raceid) %>%
      collect() %>%
      select(raceid,date,season,tech,start,
             fisid,name,rank,time,fispoints)

    ath_data <- inner_join(ath1_data,
                           ath2_data,
                           by = c('raceid','date','season','tech','start')) %>%
      left_join(winning_times,by = "raceid") %>%
      mutate(start = ifelse(start == 'Pursuit','Mass',start),
             drank = rank.y - rank.x,
             dtime = time.y - time.x,
             dfispoints = fispoints.y - fispoints.x,
             dpb = 100 * (time.y - time.x) / winning_time) %>%
      select(raceid,date,season,tech,start,fisid.x,name.x,
             fisid.y,name.y,drank,dtime,dfispoints,dpb)
  }

  ath_data$facet_name <- paste(extract_all_caps(ath1),
                               ath_data$name.y,
                               sep = " vs. ")
  ath_data[['y']] <- ath_data[[paste0('d',measure)]]

  summary_grps <- c('facet_name','date')
  if (by_tech){
    summary_grps <- c('facet_name','date','tech')
    ath_summary <- ath_data %>%
      mutate(date = paste0(substr(season,6,9),"-01-01")) %>%
      group_by_(.dots = summary_grps) %>%
      summarise(med = median(y,na.rm = TRUE))
    line_piece <- geom_line(data = ath_summary,
                            aes(x = as.Date(date),y = med,color = tech),
                            size = 1.25)
  }
  if (by_start){
    summary_grps <- c('facet_name','date','start')
    ath_summary <- ath_data %>%
      mutate(date = paste0(substr(season,6,9),"-01-01")) %>%
      group_by_(.dots = summary_grps) %>%
      summarise(med = median(y,na.rm = TRUE))
    line_piece <- geom_line(data = ath_summary,
                            aes(x = as.Date(date),y = med,color = start),
                            size = 1.25)
  }
  if (!by_start && !by_tech){
    ath_summary <- ath_data %>%
      mutate(date = paste0(substr(season,6,9),"-01-01")) %>%
      group_by_(.dots = summary_grps) %>%
      summarise(med = median(y,na.rm = TRUE))
    line_piece <- geom_line(data = ath_summary,
                            aes(x = as.Date(date),y = med),
                            color = "blue",
                            size = 1.25)
  }



  n_ath <- length(ath2)
  block1 <- data_frame(facet_name = unique(ath_data$facet_name),
                       fill_name = rep(paste(extract_all_caps(ath1),"Wins"),n_ath),
                       xmn = rep(min(ath_data$date),n_ath),
                       xmx = rep(max(ath_data$date),n_ath),
                       ymn = rep(0,n_ath),
                       ymx = rep(Inf,n_ath))
  block2 <- data_frame(facet_name = unique(ath_data$facet_name),
                       fill_name = rep("Opponent Wins",n_ath),
                       xmn = rep(min(ath_data$date),n_ath),
                       xmx = rep(max(ath_data$date),n_ath),
                       ymn = rep(-Inf,n_ath),
                       ymx = rep(0,n_ath))
  block <- bind_rows(block1,block2)
  block$xmn <- as.Date(block$xmn)
  block$xmx <- as.Date(block$xmx)

  color_label <- c('Technique','Race Type','')[c(by_tech,by_start,(!by_tech & !by_start))]
  y_label <- switch(measure,
                    "fispoints" = "Difference in FIS Points",
                    "rank" = "Difference in Finishing Place",
                    "time" = "Difference in Time",
                    "pb" = "Difference in % Back",
                    "mpb" = "Difference in MPB")

  p <- ggplot() +
    facet_wrap(~facet_name) +
    geom_rect(data = block,
              aes(NULL,NULL,
                  xmin = xmn,xmax = xmx,
                  ymin = ymn,ymax = ymx,
                  fill = factor(fill_name)),
              alpha = 0.1) +
    geom_point(data = ath_data,aes(x = as.Date(date),y = y)) +
    line_piece +
    labs(x = NULL,
         y = y_label,
         fill = "",
         color = color_label) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal") +
    guides(fill = guide_legend(override.aes = list(alpha = 0.5)))

  return(list(plot = p,
              data = ath_data,
              line_data = ath_summary,
              block = block))
}

#' Head-to-head Sprint Plot & Data
#'
#' @param ath1 character; athlete of interest
#' @param ath2 character vector of athletes to compare with
#' @param races character; one of "maj_int" or "fis"
#' @param measure character; one of "fispoints" or "rank"
#' @param by_tech boolean; split trend lines by technique.
#' @export
hth_spr <- function(ath1,
                    ath2,
                    races = c("maj_int","fis"),
                    measure = c("fispoints","rank"),
                    by_tech = FALSE){
  src <- src_sqlite(path = statskier2:::sqlite_path,create = FALSE)

  races <- match.arg(races)
  measure <- match.arg(measure)

  if (races == "maj_int"){
    ath1_data <- tbl(src = src,"main") %>%
      filter(name == ath1 &
               cat1 %in% MAJ_INT &
               type == 'Sprint') %>%
      collect() %>%
      select(raceid,date,season,tech,
             fisid,name,rank,fispoints)

    ath2_data <- tbl(src = src,"main") %>%
      filter(name %in% ath2 &
               cat1 %in% MAJ_INT &
               type == 'Sprint') %>%
      collect() %>%
      select(raceid,date,season,tech,
             fisid,name,rank,fispoints)

    ath_data <- inner_join(ath1_data,
                           ath2_data,
                           by = c('raceid','date','season','tech')) %>%
      mutate(drank = rank.y - rank.x,
             dfispoints = fispoints.y - fispoints.x) %>%
      select(raceid,date,season,tech,fisid.x,name.x,
             fisid.y,name.y,drank,dfispoints)
  }
  if (races == "fis"){
    ath1_data <- tbl(src = src,"main") %>%
      filter(name == ath1 &
               type == 'Sprint') %>%
      collect() %>%
      select(raceid,date,season,tech,
             fisid,name,rank,fispoints)

    ath2_data <- tbl(src = src,"main") %>%
      filter(name %in% ath2 &
               type == 'Sprint') %>%
      collect() %>%
      select(raceid,date,season,tech,
             fisid,name,rank,fispoints)

    ath_data <- inner_join(ath1_data,
                           ath2_data,
                           by = c('raceid','date','season','tech')) %>%
      mutate(drank = rank.y - rank.x,
             dfispoints = fispoints.y - fispoints.x) %>%
      select(raceid,date,season,tech,fisid.x,name.x,
             fisid.y,name.y,drank,dfispoints)
  }

  ath_data$facet_name <- paste(extract_all_caps(ath1),
                               extract_all_caps(ath_data$name.y),
                               sep = " vs. ")
  ath_data[['y']] <- ath_data[[paste0('d',measure)]]

  summary_grps <- c('facet_name','date')
  if (by_tech){
    summary_grps <- c('facet_name','date','tech')
    ath_summary <- ath_data %>%
      mutate(date = paste0(substr(season,6,9),"-01-01")) %>%
      group_by_(.dots = summary_grps) %>%
      summarise(med = median(y,na.rm = TRUE))
    line_piece <- geom_line(data = ath_summary,
                            aes(x = as.Date(date),y = med,color = tech),
                            size = 1.25)
  }else{
    ath_summary <- ath_data %>%
      mutate(date = paste0(substr(season,6,9),"-01-01")) %>%
      group_by_(.dots = summary_grps) %>%
      summarise(med = median(y,na.rm = TRUE))
    line_piece <- geom_line(data = ath_summary,
                            aes(x = as.Date(date),y = med),
                            color = "blue",
                            size = 1.25)
  }



  n_ath <- length(ath2)
  block1 <- data_frame(facet_name = unique(ath_data$facet_name),
                       fill_name = rep(paste(extract_all_caps(ath1),"Wins"),n_ath),
                       xmn = rep(min(ath_data$date),n_ath),
                       xmx = rep(max(ath_data$date),n_ath),
                       ymn = rep(0,n_ath),
                       ymx = rep(Inf,n_ath))
  block2 <- data_frame(facet_name = unique(ath_data$facet_name),
                       fill_name = rep("Opponent Wins",n_ath),
                       xmn = rep(min(ath_data$date),n_ath),
                       xmx = rep(max(ath_data$date),n_ath),
                       ymn = rep(-Inf,n_ath),
                       ymx = rep(0,n_ath))
  block <- bind_rows(block1,block2)
  block$xmn <- as.Date(block$xmn)
  block$xmx <- as.Date(block$xmx)

  color_label <- c('Technique','')[c(by_tech,!by_tech)]

  p <- ggplot() +
    facet_wrap(~facet_name) +
    geom_rect(data = block,
              aes(NULL,NULL,
                  xmin = xmn,xmax = xmx,
                  ymin = ymn,ymax = ymx,
                  fill = factor(fill_name)),
              alpha = 0.1) +
    geom_point(data = ath_data,aes(x = as.Date(date),y = y)) +
    line_piece +
    labs(x = NULL,
         y = NULL,
         fill = "",
         color = color_label) +
    theme(legend.position = "bottom",
          legend.direction = "horizontal") +
    guides(fill = guide_legend(override.aes = list(alpha = 0.5)))

  return(list(plot = p,
              data = ath_data,
              line_data = ath_summary,
              block = block))
}

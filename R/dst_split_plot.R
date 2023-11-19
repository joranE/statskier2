#' Distance Split Times Plot
#'
#' @importFrom egg ggarrange
#' @export
dst_split_plot <- function(.eventid,n_skiers = 30,n_seg = 6,
                           labels = TRUE,omit_splits = NULL,ref_split = 1){

  race_info <- tbl(src = ..statskier_pg_con..,
                   dbplyr::in_schema("public","v_distance_splits")) |>
    filter(eventid == .eventid) |>
    select(eventid,date,season,location,tech,length,gender,format) |>
    distinct() |>
    collect()

  if (ref_split == 1){
    ref_split_fun <- function(x,na.rm = TRUE,ref_split){
      min(x,na.rm = na.rm)
    }
  } else{
    ref_split_fun <- function(x,na.rm = TRUE,ref_split){
      sort(x,decreasing = FALSE,na.last = na.rm)[ref_split]
    }
  }

  title <- paste(race_info$date,race_info$location,race_info$gender,
                 paste0(race_info$length,"km"),race_info$tech,race_info$format)

  dst_race <- tbl(src = ..statskier_pg_con..,
                  dbplyr::in_schema("public","v_distance_splits")) |>
    filter(eventid == .eventid & !is.na(split_km)) |>
    collect() |>
    group_by(split_km) |>
    mutate(time_back = split_time - ref_split_fun(split_time,na.rm = TRUE,ref_split),
           pct_back = time_back / ref_split_fun(split_time,na.rm = TRUE,ref_split),
           name = stringr::str_trim(name,side = "both")) |>
    ungroup()

  if (race_info$format == "Pursuit"){
    dst_race <- dst_race |>
      filter(split_km != min(split_km,na.rm = TRUE))
  }

  if (!is.null(omit_splits)){
    dst_race <- dst_race |>
      filter(split_km %ni% omit_splits)
  }

  top_end <- dst_race |>
    filter(split_km == max(split_km,na.rm = TRUE) &
             split_rank <= n_skiers) |>
    mutate(name2 = extract_all_caps(name))
  top_begin <- dst_race |>
    filter(split_km == min(split_km,na.rm = TRUE) &
             name %in% top_end$name) |>
    mutate(name2 = extract_all_caps(name))
  top_seg <- dst_race |>
    group_by(name) |>
    arrange(split_km) |>
    mutate(seg_time = c(split_time[1],diff(split_time))) |>
    group_by(split_km) |>
    mutate(seg_rank = min_rank(seg_time)) |>
    as.data.frame() |>
    filter(seg_rank <= n_seg)

  subt <- NULL

  p_top <- dst_race |>
    filter(name %in% top_end$name) |>
    mutate(facet_label = "Percent Back by Split") |>
    ggplot(data = _,aes(x = split_km,y = pct_back,group = name)) +
    facet_wrap(~facet_label) +
    geom_line(alpha = 0.5)
  if (labels){
    p_top <- p_top +
      geom_text(data = top_begin,aes(label = name2),hjust = 1.1,size = 2,color = "blue") +
      geom_text(data = top_end,aes(label = name2),hjust = -0.1,size = 2,color = "blue") +
      geom_text(data = top_seg,aes(label = seg_rank),size = 2.5,color = "red")
    subt <- "Only shows top %s finishers.\nRed numbers indicate %s fastest splits over that section (may not be a person in the top %s)."
    subt <- sprintf(subt,n_skiers,n_seg,n_skiers)
  }
  p_top <- p_top +
    scale_x_continuous(breaks = unique(dst_race$split_km),expand = expansion(mult = 0.1)) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = NULL,y = "Percent Back") +
    ggtitle(label = title,
            subtitle = subt) +
    theme_bw()

  p_bottom <- dst_race |>
    filter(name %in% top_end$name) |>
    mutate(facet_label = "Race Position by Split") |>
    ggplot(data = _,aes(x = split_km,y = split_rank,group = name)) +
    facet_wrap(~facet_label) +
    geom_line(alpha = 0.5)
  if (labels){
    p_bottom <- p_bottom +
      geom_text(data = top_begin,aes(label = name2),hjust = 1.1,size = 2,color = "blue") +
      geom_text(data = top_end,aes(label = name2),hjust = -0.1,size = 2,color = "blue") +
      geom_text(data = top_seg,aes(label = seg_rank),size = 2.5,color = "red")
  }
  p_bottom <- p_bottom +
    scale_x_continuous(breaks = unique(dst_race$split_km),expand = expansion(mult = 0.1)) +
    labs(x = "Km",y = "Position",caption = "@statskier - statisticalskier.com") +
    theme_bw()

  final_plot <- egg::ggarrange(p_top,p_bottom,nrow = 2,draw = FALSE)
  return(list(final_plot = final_plot,top = p_top,bottom = p_bottom,data = dst_race))
}

#' Distance Pursuit Combined Split Times Plot
#'
#' @importFrom egg ggarrange
#' @export
dst_pur_split_plot <- function(.eventid,n_skiers = 30,n_seg = 6,
                           labels = TRUE,omit_splits = NULL,ref_split = 1){

  pur_lnk <- tbl(src = ..statskier_pg_con..,dbplyr::in_schema('public','dst_pur_link')) |>
    filter(eventid == .eventid) |>
    collect()
  ev_ids <- c(pur_lnk$eventid,pur_lnk$pur_eventid)
  ev_info <- tbl(src = ..statskier_pg_con..,in_schema('public','v_event')) |>
    filter(eventid %in% ev_ids) |>
    collect() |>
    mutate(race_order = if_else(eventid == .eventid,2L,1L)) |>
    arrange(race_order)
  split_km_adjustment <- ev_info$length[1]
  first_leg_winning_time <- tbl(src = ..statskier_pg_con..,
                               dbplyr::in_schema('public','v_distance')) |>
    filter(eventid == local(ev_ids[2]) & !is.na(rank) & rank == 1) |>
    select(time) |>
    collect()
  split_time_adjustment <- unique(first_leg_winning_time$time)

  race_info <- ev_info |>
    filter(eventid %in% ev_ids) |>
    select(date,season,location,tech,length,gender,format) |>
    summarise(across(everything(),.fns = ~paste(unique(.x),collapse = '/')))

  if (ref_split == 1){
    ref_split_fun <- function(x,na.rm = TRUE,ref_split){
      min(x,na.rm = na.rm)
    }
  } else{
    ref_split_fun <- function(x,na.rm = TRUE,ref_split){
      sort(x,decreasing = FALSE,na.last = na.rm)[ref_split]
    }
  }

  title <- paste(race_info$date,race_info$location,race_info$gender,
                 paste0(race_info$length,"km"),race_info$tech,race_info$format)

  dst_race <- tbl(src = ..statskier_pg_con..,
                  dbplyr::in_schema("public","v_distance_splits")) |>
    filter(eventid %in% ev_ids & !is.na(split_km)) |>
    collect() |>
    left_join(ev_info |> select(eventid,race_order),by = 'eventid')

  dst_race <- dst_race |>
    group_by(compid) |>
    mutate(split_km = if_else(race_order == 2L,
                              split_km + split_km_adjustment,
                              split_km),
           split_time = if_else(race_order == 2L,
                                split_time + split_time_adjustment,
                                split_time)) |>
    arrange(compid,split_km) |>
    group_by(split_km) |>
    mutate(time_back = split_time - ref_split_fun(split_time,na.rm = TRUE,ref_split),
           pct_back = time_back / ref_split_fun(split_time,na.rm = TRUE,ref_split),
           name = stringr::str_trim(name,side = "both")) |>
    ungroup()

  if (race_info$format == "Pursuit"){
    dst_race <- dst_race |>
      filter(split_km != min(split_km,na.rm = TRUE))
  }

  if (!is.null(omit_splits)){
    dst_race <- dst_race |>
      filter(split_km %ni% omit_splits)
  }

  top_end <- dst_race |>
    filter(split_km == max(split_km,na.rm = TRUE) &
             split_rank <= n_skiers) |>
    mutate(name2 = extract_all_caps(name))
  top_begin <- dst_race |>
    filter(split_km == min(split_km,na.rm = TRUE) &
             name %in% top_end$name) |>
    mutate(name2 = extract_all_caps(name))
  top_seg <- dst_race |>
    group_by(name) |>
    arrange(split_km) |>
    mutate(seg_time = c(split_time[1],diff(split_time))) |>
    group_by(split_km) |>
    mutate(seg_rank = min_rank(seg_time)) |>
    as.data.frame() |>
    filter(seg_rank <= n_seg)

  subt <- NULL

  p_top <- dst_race |>
    filter(name %in% top_end$name) |>
    mutate(facet_label = "Percent Back by Split") |>
    ggplot(data = _,aes(x = split_km,y = pct_back,group = name)) +
    facet_wrap(~facet_label) +
    geom_line(alpha = 0.5)
  if (labels){
    p_top <- p_top +
      geom_text(data = top_begin,aes(label = name2),hjust = 1.1,size = 2,color = "blue") +
      geom_text(data = top_end,aes(label = name2),hjust = -0.1,size = 2,color = "blue") +
      geom_text(data = top_seg,aes(label = seg_rank),size = 2.5,color = "red")
    subt <- "Only shows top %s finishers.\nRed numbers indicate %s fastest splits over that section (may not be a person in the top %s)."
    subt <- sprintf(subt,n_skiers,n_seg,n_skiers)
  }
  p_top <- p_top +
    scale_x_continuous(breaks = unique(dst_race$split_km),expand = expansion(mult = 0.1)) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = NULL,y = "Percent Back") +
    ggtitle(label = title,
            subtitle = subt) +
    theme_bw()

  p_bottom <- dst_race |>
    filter(name %in% top_end$name) |>
    mutate(facet_label = "Race Position by Split") |>
    ggplot(data = _,aes(x = split_km,y = split_rank,group = name)) +
    facet_wrap(~facet_label) +
    geom_line(alpha = 0.5)
  if (labels){
    p_bottom <- p_bottom +
      geom_text(data = top_begin,aes(label = name2),hjust = 1.1,size = 2,color = "blue") +
      geom_text(data = top_end,aes(label = name2),hjust = -0.1,size = 2,color = "blue") +
      geom_text(data = top_seg,aes(label = seg_rank),size = 2.5,color = "red")
  }
  p_bottom <- p_bottom +
    scale_x_continuous(breaks = unique(dst_race$split_km),expand = expansion(mult = 0.1)) +
    labs(x = "Km",y = "Position",caption = "@statskier - statisticalskier.com") +
    theme_bw()

  final_plot <- egg::ggarrange(p_top,p_bottom,nrow = 2,draw = FALSE)
  return(list(final_plot = final_plot,top = p_top,bottom = p_bottom,data = dst_race))
}

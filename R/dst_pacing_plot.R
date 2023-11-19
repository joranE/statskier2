#' @export
dst_pacing_plot <- function(.eventid,
                            .subset,
                            n_laps = NULL,
                            omit_splits = NULL,
                            title_text = "",
                            subtitle_text = "Percent behind the fastest time on the preceding lap segment",
                            x_axis_lab = "Lap Split Point",
                            y_axis_lab = "Percent Back on Race Segment",
                            .measure = "percent_back",
                            collapse_short_seg = FALSE,
                            skip_first_split = FALSE){

  if (n_laps <= 8){
    cs <- scale_color_brewer(palette = "Set2")
  } else {
    cs <- NULL
  }

  .measure <- match.arg(arg = .measure,
                        choices = c("time_back","percent_back","time_back_km"))
  y_var <- switch(EXPR = .measure,
                  time_back = "seg_tb",
                  percent_back = "seg_pb",
                  time_back_km = "seg_tb_km")
  y_var <- rlang::sym(y_var)

  y_scale <- switch(EXPR = .measure,
                    time_back = scale_y_continuous(),
                    percent_back = scale_y_continuous(labels = scales::percent_format()),
                    time_back_km = scale_y_continuous())

  #Pull split times
  splits <- tbl(src = ..statskier_pg_con..,
                dbplyr::in_schema("public","v_distance_splits")) |>
    filter(eventid == .eventid & !is.na(split_km)) |>
    collect() |>
    select(eventid,compid,name,nation,split_km,split_rank,split_time)

  splits <- splits |>
    complete(compid,split_km) |>
    tidyr::fill(eventid,name,nation,.direction = 'downup')

  if (!is.null(omit_splits)){
    splits <- splits |>
      filter(split_km %ni% omit_splits)
  }

  #How many split time points are there?
  n_splits <- n_distinct(splits$split_km)

  splits <- splits |>
    group_by(compid) |>
    arrange(compid,split_km) |>
    mutate(lap = rep(seq_len(n_laps),each = ceiling(n_splits / n_laps),length.out = n()),
           seg_len = c(split_km[1],diff(split_km)),
           seg_time = c(split_time[1],diff(split_time)))


  if (collapse_short_seg){
    splits <- splits |>
      mutate(too_short = lead(seg_len < 0.4,1)) |>
      tidyr::replace_na(replace = list(too_short = FALSE)) |>
      filter(!too_short)

    #How many split time points are there?
    n_splits <- n_distinct(splits$split_km)

    splits <- splits |>
      group_by(compid) |>
      arrange(compid,split_km) |>
      mutate(lap = rep(seq_len(n_laps),each = ceiling(n_splits / n_laps),length.out = n()),
             seg_len = c(split_km[1],diff(split_km)),
             seg_time = c(split_time[1],diff(split_time)))
  }

  #Calculate laps, segments
  splits <- splits |>
    group_by(compid,lap) |>
    arrange(split_km) |>
    mutate(lap_pt = rank(split_km)) |>
    group_by(lap,lap_pt) |>
    mutate(seg_tb = seg_time - min(seg_time,na.rm = TRUE),
           seg_pb = (seg_time - min(seg_time,na.rm = TRUE)) / min(seg_time,na.rm = TRUE)) |>
    as.data.frame() |>
    mutate(seg_tb_km = seg_tb / seg_len)



  #Official race results & focus subset
  fin_results <- tbl(src = ..statskier_pg_con..,
                     dbplyr::in_schema("public","v_distance")) |>
    filter(eventid == .eventid) |>
    collect() |>
    mutate_if(.predicate = bit64::is.integer64,.funs = as.integer)
  .subset <- rlang::enquo(.subset)
  race_subset <- fin_results |>
    filter(!!.subset) |>
    select(eventid,compid,rank)

  #Construct x axis tick mark labels out of split_km
  x_labs <- paste0(sort(unique(splits$split_km)),"k")
  n <- length(x_labs) / n_laps
  x_labs <- split(x_labs,rep(seq_len(n),times = n_laps))
  x_labs <- mapply(FUN = paste,x_labs,MoreArgs = list(collapse = "\n"))


  #Grab subset and graph
  m1 <- inner_join(splits,
                   race_subset,by = "compid") |>
    mutate(name = forcats::fct_reorder(name,rank))

  if (skip_first_split){
    m1 <- m1 |>
      filter(!(lap == 1 & lap_pt == 1))
  }

  p <- ggplot(data = m1,
              aes(x = lap_pt,y = !!y_var,color = factor(lap),group = lap)) +
    facet_wrap(~name) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = seq_along(x_labs),labels = x_labs) +
    y_scale +
    cs +
    labs(x = x_axis_lab,
         y = y_axis_lab,
         color = "Lap",
         caption = "@statskier - statisticalskier.com") +
    ggtitle(label = title_text,
            subtitle = subtitle_text) +
    theme_bw()
  return(list(plot = p,
              data = m1,
              x_labs = x_labs,
              y_scale = y_scale,
              x_axis_lab = x_axis_lab,
              y_axis_lab = y_axis_lab,
              title_text = title_text,
              subtitle_text = subtitle_text))
}

#' @export
dst_pur_pacing_plot <- function(.eventid,
                            .subset,
                            n_laps = NULL,
                            omit_splits = NULL,
                            title_text = "",
                            subtitle_text = "Percent behind the fastest time on the preceding lap segment",
                            x_axis_lab = "Lap Split Point",
                            y_axis_lab = "Percent Back on Race Segment",
                            .measure = "percent_back",
                            collapse_short_seg = FALSE,
                            skip_first_split = FALSE){

  if (n_laps <= 8){
    cs <- scale_color_brewer(palette = "Set2")
  } else {
    cs <- NULL
  }

  .measure <- match.arg(arg = .measure,
                        choices = c("time_back","percent_back","time_back_km"))
  y_var <- switch(EXPR = .measure,
                  time_back = "seg_tb",
                  percent_back = "seg_pb",
                  time_back_km = "seg_tb_km")
  y_var <- rlang::sym(y_var)

  y_scale <- switch(EXPR = .measure,
                    time_back = scale_y_continuous(),
                    percent_back = scale_y_continuous(labels = scales::percent_format()),
                    time_back_km = scale_y_continuous())

  pur_lnk <- tbl(src = ..statskier_pg_con..,
                 dbplyr::in_schema('public','dst_pur_link')) |>
    filter(eventid == .eventid) |>
    collect()
  ev_ids <- c(pur_lnk$eventid,pur_lnk$pur_eventid)
  ev_info <- tbl(src = ..statskier_pg_con..,in_schema('public','v_event')) |>
    filter(eventid %in% ev_ids) |>
    collect() |>
    mutate(race_order = if_else(eventid == .eventid,2L,1L)) |>
    arrange(race_order)

  #Pull split times
  splits <- tbl(src = ..statskier_pg_con..,
                dbplyr::in_schema("public","v_distance_splits")) |>
    filter(eventid %in% ev_ids & !is.na(split_km)) |>
    collect() |>
    select(eventid,compid,name,nation,split_km,split_rank,split_time) |>
    left_join(ev_info |> select(eventid,race_order),by = 'eventid')

  splits <- splits |>
    group_by(compid) |>
    mutate(split_km = if_else(race_order == 2L,
                              split_km + split_km_adjustment,
                              split_km),
           split_time = if_else(race_order == 2L,
                                split_time + split_time_adjustment,
                                split_time)) |>
    arrange(compid,split_km)

  if (!is.null(omit_splits)){
    splits <- splits |>
      filter(split_km %ni% omit_splits)
  }

  #How many split time points are there?
  n_splits <- n_distinct(splits$split_km)

  splits <- splits |>
    group_by(compid) |>
    arrange(compid,split_km) |>
    mutate(lap = rep(seq_len(n_laps),each = ceiling(n_splits / n_laps),length.out = n()),
           seg_len = c(split_km[1],diff(split_km)),
           seg_time = c(split_time[1],diff(split_time)))


  if (collapse_short_seg){
    splits <- splits |>
      mutate(too_short = lead(seg_len < 0.4,1)) |>
      tidyr::replace_na(replace = list(too_short = FALSE)) |>
      filter(!too_short)

    #How many split time points are there?
    n_splits <- n_distinct(splits$split_km)

    splits <- splits |>
      group_by(compid) |>
      arrange(compid,split_km) |>
      mutate(lap = rep(seq_len(n_laps),each = ceiling(n_splits / n_laps),length.out = n()),
             seg_len = c(split_km[1],diff(split_km)),
             seg_time = c(split_time[1],diff(split_time)))
  }

  #Calculate laps, segments
  splits <- splits |>
    group_by(compid,lap) |>
    arrange(split_km) |>
    mutate(lap_pt = rank(split_km)) |>
    group_by(lap,lap_pt) |>
    mutate(seg_tb = seg_time - min(seg_time,na.rm = TRUE),
           seg_pb = (seg_time - min(seg_time,na.rm = TRUE)) / min(seg_time,na.rm = TRUE)) |>
    as.data.frame() |>
    mutate(seg_tb_km = seg_tb / seg_len)



  #Official race results & focus subset
  fin_results <- tbl(src = ..statskier_pg_con..,
                     dbplyr::in_schema("public","v_distance")) |>
    filter(eventid == .eventid) |>
    collect() |>
    mutate_if(.predicate = bit64::is.integer64,.funs = as.integer)
  .subset <- rlang::enquo(.subset)
  race_subset <- fin_results |>
    filter(!!.subset) |>
    select(eventid,compid,rank)

  #Construct x axis tick mark labels out of split_km
  x_labs <- paste0(sort(unique(splits$split_km)),"k")
  n <- length(x_labs) / n_laps
  x_labs <- split(x_labs,rep(seq_len(n),times = n_laps))
  x_labs <- mapply(FUN = paste,x_labs,MoreArgs = list(collapse = "\n"))


  #Grab subset and graph
  m1 <- inner_join(splits,
                   race_subset,by = "compid") |>
    mutate(name = forcats::fct_reorder(name,rank))

  if (skip_first_split){
    m1 <- m1 |>
      filter(!(lap == 1 & lap_pt == 1))
  }

  p <- ggplot(data = m1,
              aes(x = lap_pt,
                  y = !!y_var,
                  color = factor(lap),
                  group = lap)) +
    facet_wrap(~name) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = seq_along(x_labs),labels = x_labs) +
    y_scale +
    cs +
    labs(x = x_axis_lab,
         y = y_axis_lab,
         color = "Lap",
         caption = "@statskier - statisticalskier.com") +
    ggtitle(label = title_text,
            subtitle = subtitle_text) +
    theme_bw()
  return(p)
}

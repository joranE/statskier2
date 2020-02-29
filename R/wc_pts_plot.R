# src_dst_maj_int <- tbl(conl,"v_distance_maj_int")
# src_spr_maj_int <- tbl(conl,"v_sprint_maj_int")
# src_stg_maj_int <- tbl(conl,"v_stage_maj_int")

# dst_wc_pts <- src_dst_maj_int %>%
#   filter(season == "2018-2019" & !is.na(wc_pts)) %>%
#   mutate(event_type = 'Distance') %>%
#   select(event_type,eventid,season,date,location,cat1,gender,compid,fisid,name,nation,wc_pts) %>%
#   collect()
# spr_wc_pts <- src_spr_maj_int %>%
#   filter(season == "2018-2019" & !is.na(wc_pts)) %>%
#   mutate(event_type = 'Sprint') %>%
#   select(event_type,eventid,season,date,location,cat1,gender,compid,fisid,name,nation,wc_pts) %>%
#   collect()
# stg_wc_pts <- src_stg_maj_int %>%
#   filter(season == "2018-2019" & !is.na(wc_pts)) %>%
#   mutate(event_type = 'Stage',
#          location = NA) %>%
#   select(event_type,eventid,season,date,location,cat1,gender,compid,fisid,name,nation,wc_pts) %>%
#   collect()
#
# wc_pts <- bind_rows(dst_wc_pts,
#                     spr_wc_pts,
#                     bind_rows(dst_wc_pts,spr_wc_pts,stg_wc_pts) %>% mutate(event_type = "Overall")) %>%
#   group_by(event_type,gender,compid,fisid,name) %>%
#   arrange(date) %>%
#   mutate(cum_wc_pts = cumsum(wc_pts)) %>%
#   ungroup() %>%
#   mutate(event_type = factor(event_type,levels = c("Sprint","Distance","Overall")))
#
# wc_pts_tot <- wc_pts %>%
#   group_by(event_type,gender,compid,fisid,name) %>%
#   summarise(tot_pts = sum(wc_pts)) %>%
#   arrange(event_type,gender,desc(tot_pts)) %>%
#   group_by(event_type,gender) %>%
#   mutate(pts_rnk = rank(-tot_pts))
#
# wc_pts %>%
#   semi_join(wc_pts_tot %>% filter(pts_rnk <= 10),by = c("event_type","gender","compid")) %>%
#   ggplot(data = .,aes(x = as.Date(date),y = cum_wc_pts,group = name)) +
#     facet_grid(gender~event_type) +
#     geom_line() +
#     geom_point() +
#     theme_bw()


# library(statskier2)
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# ski_src <- tbl(src = options()$statskier_src,"main")
#
# medalists <- ski_src %>%
#   filter(cat1 %in% c('OWG','WSC','WC','TDS') &
#            type != 'Stage' &
#            rank <= 3 &
#            season >= '2008-2009') %>%
#   collect()
#
# repeat_medalists <- medalists %>%
#   group_by(gender,type,fisid,compid,name) %>%
#   mutate(n_pod = n()) %>%
#   ungroup() %>%
#   filter(n_pod > 1)
#
# repeat_medalists %>%
#   group_by(gender,type) %>%
#   summarise(n = n_distinct(fisid))
#
# rep_med_cat <- repeat_medalists %>%
#   select(gender,type,fisid,compid,name,n_pod) %>%
#   distinct() %>%
#   arrange(gender,compid,type)
#
# rep_med_results <- ski_src %>%
#   filter(compid %in% repeat_medalists$compid &
#            type != 'Stage' &
#            !is.na(fispoints)) %>%
#   collect() %>%
#   semi_join(rep_med_cat,by = c('gender','type','compid'))
#
# rep_med_age <- rep_med_results %>%
#   group_by(gender,type,age) %>%
#   #do(boot = bs_median(vec = .$fispoints)) %>%
#   mutate(lower = quantile(fispoints,0.1,na.rm = TRUE),
#          mid = median(fispoints,na.rm = TRUE),
#          upper = quantile(fispoints,0.9,na.rm = TRUE)) %>%
#   ungroup()
#
# rep_med_age %>%
#   filter(age <= 35) %>%
#   ggplot(data = .,aes(x = age)) +
#     facet_grid(gender~type) +
#     geom_ribbon(aes(ymin = lower,ymax = upper),alpha = 0.25) +
#     geom_line(aes(y = mid),color = "blue")
#
# rep_lag5 <- rep_med_results %>%
#   arrange(gender,type,compid,date) %>%
#   group_by(gender,type,compid) %>%
#   mutate(fp_lag = lag_best_n(x = fispoints,index = date))
#
# rep_lag5 %>%
#   group_by(gender,type,age) %>%
#   summarise(lower = quantile(fp_lag,0.1,na.rm = TRUE),
#             mid = quantile(fp_lag,0.5,na.rm = TRUE),
#             upper = quantile(fp_lag,0.9,na.rm = TRUE)) %>%
#   filter(age >= 18 & age <= 30) %>%
#   ggplot(data = .,aes(x = age)) +
#     facet_grid(gender~type) +
#     geom_ribbon(aes(ymin = lower,ymax = upper),alpha = 0.25) +
#     geom_line(aes(y = mid),color = "blue") +
#     scale_x_continuous(breaks = 18:30) +
#     theme_bw()

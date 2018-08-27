#' Athlete Development Trend
#'
#' Plot athlete development relative to a cohort of top athletes at equivalent
#' ages.
#'
#' @param ath character vector of athlete names
#' @param top_k integer; cohort includes athletes finishing in the `top_k`
#' @param times integer; cohort includes athletes finishing in the `top_k` at
#' least `times` times.
#' @param ci_lims named vector defining CI limits for estimating the median
#' @import squr
#' @export
dev_cohort <- function(ath,top_k = 5,times = 10,
                       ci_lims = c(min = 0.1,max = 0.9)){
  main <- tbl(src = options()$statskier_src,"main")
  conl <- db_xc_local()

  cohort_data <- sq_file("sql/cohort.sql") %>%
    sq_set(rank = top_k,
           n_result = times) %>%
    sq_send(.with = ss_query_,con = conl)

  cohort_trend <- cohort_data %>%
    group_by(gender,type,compid,fisid,age) %>%
    summarise(n = n(),
              mid = median(fispoints,na.rm = TRUE)) %>%
    group_by(gender,type,age) %>%
    summarise(n = n(),
              co_lower = quantile(mid,probs = ci_lims["min"],na.rm = TRUE),
              co_mid_ind = median(mid,na.rm = TRUE),
              co_upper = quantile(mid,probs = ci_lims["max"],na.rm = TRUE))

  ath_data <- main %>%
    filter(name %in% ath &
             type != 'Stage' &
             !is.na(fispoints)) %>%
    collect() %>%
    group_by(gender,type,compid,fisid,name,age) %>%
    do(boot = bs_median(vec = .$fispoints)) %>%
    mutate(lower = quantile(boot$bs,ci_lims["min"],na.rm = TRUE),
           mid_ind = median(boot$bs,na.rm = TRUE),
           upper = quantile(boot$bs,ci_lims["max"],na.rm = TRUE)) %>%
    left_join(cohort_trend,by = c('gender','type','age')) %>%
    filter(!is.na(mid_ind))

  ath_age_range <- range(ath_data$age)

  title <- sprintf("Median FIS Points vs age")
  subtitle <- sprintf("Shaded area = athletes finishing in the top %s\nat least %s times in major international races.",top_k,times)

  p <- ggplot(data = ath_data) +
    facet_grid(name~type) +
    geom_ribbon(aes(x = age,ymin = co_lower,ymax = co_upper),alpha = 0.5) +
    geom_pointrange(data = ath_data,
                    aes(x = age,ymin = lower,y = mid_ind,ymax = upper),
                    size = 0.25,
                    color = "blue") +
    labs(x = "Age",y = "FIS Points") +
    ggtitle(label = title,subtitle = subtitle) +
    coord_cartesian(xlim = ath_age_range)

  return(list(plot = p,
              cohort_data = cohort_data,
              ath_data = ath_data))
}

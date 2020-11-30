#' Assess Head-to-Head Performance Using lmer Model
#'
#' Fit an `lmer()` model to prior head-to-head results for a set of skiers
#' in order to plot expected vs observed performances. This is attempting to
#' serve the same general purpose as `head_to_head_race()` but using a more
#' "sophisticated" method.
#'
#' @param ath_names character vector of athlete names
#' @param event_id integer; id of the race to assess performance of
#' @param num_opp integer; number of opponents to consider in each race, if `Inf`
#' use all opponents
#' @param cutoff integer; size of time window in days to collect prior races from
#' @param min_encounters integer; only include opponents who have faced an athlete
#' at least this many times
#' @param measure character; one of "rank", "fispints" or "pb" (percent back)
#' @param restrict_by currently unused
#' @param race_title character; race title to add to the plot
#' @importFrom tidyr nest unnest
#' @importFrom lme4 lmer
#' @export
hth_lmer <- function(ath_names,
                     event_id,
                     num_opp = Inf,
                     cutoff = 365 * 5,
                     min_encounters = 1,
                     measure = c('rank','fispoints','pb'),
                     events = c('all','maj_int'),
                     restrict_by = NULL,
                     race_title = ""){
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
             type == race_info$type &
             date <= race_info$date) %>%
    mutate(key = paste(ath_name,opp_name))

  if (events == 'maj_int'){
    hth_df <- filter(hth_df,cat1 %in% c('WC','TDS','OWG','WSC'))
  }

  hth_df$y <- hth_df[[paste0("diff_",measure)]]
  ylab <- switch(measure,
                 'rank' = 'Finishing Place',
                 'fispoints' = 'FIS Points',
                 'pb' = 'Percent Back')

  mod_data <- filter(hth_df,raceid != race_id)
  w1 <- mod_data$n_races / sum(mod_data$n_races)
  w2 <- 1 / (race_day - as.integer(as.Date(mod_data$date)))
  w <- w2
  mod_data$weight <- w
  pred_data <- hth_df %>%
    filter(raceid == race_id &
             key %in% unique(mod_data$key))


  m <- mod_data %>%
    group_by(ath_name) %>%
    do(mod = lmer(diff_pb~length+tech+(1|opp_name),
                  data = .,
                  weights = .$weight))
  pred_data_nest <- pred_data %>%
    group_by(ath_name) %>%
    nest()

  m <- left_join(m,pred_data_nest,by = 'ath_name')
  p <- m %>%
    rowwise() %>%
    do(preds = predict(object = .$mod,newdata = .$data))
  m <- bind_cols(m,p)

  pred_final <- m %>%
    select(-mod) %>%
    unnest()


  plot <- ggplot(data = pred_final,aes(x = y,y = preds)) +
    facet_wrap(~ath_name) +
    geom_point() +
    geom_abline(intercept = 0,slope = 1) +
    labs(x = "Observed Difference In % Back",
         y = "Expected Difference In % Back",
         caption = "@statskier - statisticalskierdata.com") +
    ggtitle(label = paste("Expected vs Observed Performance",race_title,sep = " - "),
            subtitle = "Each point is one opponent in the race. Stronger opponents in the lower left, weaker in the upper right.\nMore points to the right/below the line is better.")

  return(list(preds = pred_final,model = m,plot = plot))
}

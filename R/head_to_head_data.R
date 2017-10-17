#' @export
hth_data <- function(athletes,opponents){
  conl <- db_xc_local()
  hth <- sq_file("sql/hth_data.sql") %>%
    sq_set(ath_name = IN(athletes),
           opp_name = IN(opponents)) %>%
    sq_send(.with = ss_query_,conl)
  dbDisconnect(conl)

  winning_times <- tbl(src = options()$statskier_src,"main") %>%
    filter(raceid %in% hth$raceid & rank == 1) %>%
    select(raceid,winning_time = time) %>%
    distinct() %>%
    collect()

  hth <- left_join(hth,
                   winning_times,
                   by = 'raceid') %>%
    mutate(ath_pb = (ath_time - winning_time) / winning_time,
           opp_pb = (opp_time - winning_time) / winning_time) %>%
    mutate(diff_pb = opp_pb - ath_pb,
           diff_rank = opp_rank - ath_rank,
           diff_fispoints = opp_fispoints - ath_fispoints,
           diff_rankqual = opp_rankqual - ath_rankqual)

  hth <- hth %>%
    group_by(ath_compid,opp_compid) %>%
    mutate(n_races = n_distinct(raceid)) %>%
    as.data.frame()

  return(hth)
}

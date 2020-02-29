#' @export
dst_hth_data <- function(ath_compid,opp_compid){
  result <- sq_file("sql/skier_head_to_head_dst.sql") %>%
    sq_set(ath_compid = ath_compid,
           opp_compid = IN(opp_compid)) %>%
    sq_send(.with = ss_query,conl)
  result
}

#' @export
spr_hth_data <- function(ath_compid,opp_compid){
  result <- sq_file("sql/skier_head_to_head_spr.sql") %>%
    sq_set(ath_compid = ath_compid,
           opp_compid = IN(opp_compid)) %>%
    sq_send(.with = ss_query,conl)
  result
}

#' @export
skier_results <- function(.name = NULL,.fisid = NULL,.compid = NULL){
  v_dst <- dplyr::tbl(src = ..statskier_pg_con..,dbplyr::in_schema("public","v_distance"))
  v_spr <- dplyr::tbl(src = ..statskier_pg_con..,dbplyr::in_schema("public","v_sprint"))

  dst_res <- v_dst %>%
    filter(fisid %in% .fisid) %>%
    collect() %>%
    mutate_if(.predicate = bit64::is.integer64,.funs = as.integer)
  spr_res <- v_spr %>%
    filter(fisid %in% .fisid) %>%
    collect() %>%
    mutate_if(.predicate = bit64::is.integer64,.funs = as.integer)

  list(dst = dst_res,spr = spr_res)
}

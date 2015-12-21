#' @export
find_skier <- function(x){
  conl <- db_xc_local()
  q <- sprintf("select fisid,name,gender,yob,nation,count(distinct raceid) races
  from main where name like '%%%s%%' group by fisid,name,gender,yob,nation",x)
  results <- ss_query(conl,q)
  dbDisconnect(conl)
  results
}

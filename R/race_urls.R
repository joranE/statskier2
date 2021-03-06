#' Open Race URL in Browser
#'
#' Open results for a race in a web browser.
#'
#' @param raceid integer
#' @export
#' @family race info functions
#' @examples
#' \dontrun{
#' open_race_url(raceid = 7902)
#' }
open_race_url <- function(raceid){
  if (length(raceid) > 1){
    raceid <- raceid[1]
    warning("Ignoring all but first raceid...",immediate. = TRUE)
  }
  rid <- raceid
  urls <- tbl(options()$statskier_src,"race_url") %>%
    filter(raceid == rid) %>%
    collect()
  if (nrow(urls) == 0){
    stop("No URLs for that raceid yet.")
  }
  url1 <- urls$url1
  url2 <- urls$url2

  if (!is.na(url1)){
    browseURL(url = url1)
  }
  if(!is.na(url2)){
    browseURL(url = url2)
  }
}


#' Update Race URLs
#'
#' Only available to those with UPDATE priviledges for the db.
#'
#' @param raceid integer race id
#' @param url1 character
#' @param url2 character
#' @family race info functions
update_race_url <- function(raceid,url1 = NA,url2 = NA){
  con_local <- db_xc_local()
  con_remote <- db_xc_remote()
  current <- ss_query(con_local,sprintf("select * from race_url where raceid = %s",raceid))
  if (nrow(current) == 0){
    stop("That raceid isn't in race_url table.")
  }
  if (!is.na(url1)){
    q <- sprintf("update race_url set url1 = '%s' where raceid = %s",url1,raceid)
    ss_query(con_local,q)
    ss_query(con_remote,q)
  }
  if (!is.na(url2)){
    q <- sprintf("update race_url set url2 = '%s' where raceid = %s",url2,raceid)
    ss_query(con_local,q)
    ss_query(con_remote,q)
  }
}

#' Insert Race URLs
#'
#' Only available to those with INSERT priviledges
#'
#' @param raceid integer race id
#' @param url1 character
#' @param url2 character
insert_race_url <- function(raceid,url1 = NA,url2 = NA){
  con_local <- db_xc_local()
  con_remote <- db_xc_remote()
  current <- ss_query(con_local,sprintf("select * from race_url where raceid = %s",raceid))
  if (nrow(current) > 0){
    stop("That raceid already exists in race_url table.")
  }
  if (is.na(url1)){
    stop("Must provide at least url1.")
  }
  if (!is.na(url2)){
    q <- sprintf("insert into race_url values (%s,'%s','%s')",raceid,url1,url2)
    ss_query(con_local,q)
    ss_query(con_remote,q)
  }else{
    q <- sprintf("insert into race_url values (%s,'%s',NULL)",raceid,url1,url2)
    ss_query(con_local,q)
    ss_query(con_remote,q)
  }
  dbDisconnect(con_local)
  dbDisconnect(con_remote)
}

#' Races With Missing URLs
#'
#' Look for races with missing URLs
missing_race_url <- function(){

  missing_url <- left_join(tbl(src = options()$statskier_src,
                               from = sql("select distinct raceid,season,date,gender,
                                          location,type,cat1,cat2,length,tech,start
                                          from main")),
                           tbl(src,"race_url"),
                           by = "raceid") %>%
    filter(is.na(url1) & is.na(url2)) %>%
    collect() %>%
    as.data.frame()
  missing_url
}

#' Fill Missing URLs
#'
#' Utility function to fill missing race URLs.
fill_missing_url <- function(){
  todo <- missing_race_url() %>%
    arrange(season,date)
  n <- nrow(todo)

  for (i in seq_len(n)){
    rec <- todo[i,]
    print(rec)
    choice <- menu(choices = c('Continue','Skip','Exit'))
    if (choice == 2){
      next
    }
    if (choice == 3){
      break
    }else{
      if (rec$type == 'Sprint'){
        url1 <- readline(prompt = "url1: ")
        url2 <- readline(prompt = "url2: ")
        insert_race_url(raceid = rec$raceid,url1 = url1,url2 = url2)
      }else{
        url1 <- readline(prompt = "url1: ")
        insert_race_url(raceid = rec$raceid,url1 = url1)
      }
    }
  }
}

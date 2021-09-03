#' WJC & U23 History Plots
#'
#' Results trends for nations at either World Juniors or
#' U23s, by gender and race type.
#'
#' @param nations character vector of nation codes, eg "USA", "CAN", etc.
#' @param races character; one of "wjc" or "wu23"
#' @return A named list with components:
#' \enumerate{
#'  \item \code{plots} - a named list of plots, one for each nation
#'  \item \code{data} - raw data used for plots
#' }
#' @export
#' @examples
#' \dontrun{
#' p <- wjc_u23_plot(nations = c('USA','CAN'),races = 'wjc')
#' print(p$plots$USA)
#' print(p$plots$CAN)
#' }
wjc_u23_plot <- function(nations,races = c('wjc','wu23')){
  races <- match.arg(races)
  dst <- tbl(..statskier_pg_con..,dbplyr::in_schema("public","v_distance")) %>%
    filter(primary_tag == races &
             nation %in% nations) %>%
    collect()
  spr <- tbl(..statskier_pg_con..,dbplyr::in_schema("public","v_sprint")) %>%
    filter(primary_tag == races &
             nation %in% nations) %>%
    collect()

  dst_spr <- dst %>%
    mutate(event_type = "Distance") %>%
    select(event_type,season,date,event_type,gender,compid,nation,rank) %>%
    bind_rows(mutate(spr,event_type = "Sprint") %>%
                select(event_type,season,date,event_type,gender,compid,nation,rank))

  sprCutoff <- data.frame(event_type = c('Sprint','Sprint'),
                          gender = c('Men','Women'),
                          yint = c(30,30))

  dst_spr_med <- dst_spr %>%
    group_by(nation,gender,event_type,season) %>%
    summarise(med = median(rank,na.rm = TRUE)) %>%
    mutate(date = season_to_date(season))

  dst_spr <- split(dst_spr,dst_spr$nation)
  dst_spr_med <- split(dst_spr_med,dst_spr_med$nation)

  f_plot <- function(dat1,dat2){
    p <- ggplot(data = dat1,aes(x = as.Date(date),y = rank)) +
      facet_grid(gender~event_type) +
      geom_hline(data = sprCutoff,aes(yintercept = yint),color = "red") +
      geom_point() +
      geom_line(data = dat2,aes(y = med),color = "blue") +
      labs(x = NULL,y = "Finishing Place")
    return(p)
  }

  plots <- mapply(f_plot,dat1 = dst_spr,dat2 = dst_spr_med,USE.NAMES = TRUE,SIMPLIFY = FALSE)

  return(list(plots = plots,
              data = dst_spr))
}


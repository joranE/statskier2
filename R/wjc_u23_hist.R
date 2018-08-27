#' WJC & U23 History Plots
#'
#' Results trends for nations at either World Juniors or
#' U23s, by gender and race type.
#'
#' @param nations character vector of nation codes, eg "USA", "CAN", etc.
#' @param races character; one of "WJC" or "U23"
#' @return A named list with components:
#' \enumerate{
#'  \item \code{plots} - a named list of plots, one for each nation
#'  \item \code{data} - raw data used for plots
#' }
#' @export
#' @examples
#' \dontrun{
#' p <- wjc_u23_plot(nations = c('USA','CAN'),races = 'WJC')
#' print(p$plots$USA)
#' print(p$plots$CAN)
#' }
wjc_u23_plot <- function(nations,races = c('WJC','U23')){

  races <- match.arg(races)
  wjc <- tbl(src = options()$statskier_src,"main") %>%
    filter(cat1 == races &
             nation %in% nations) %>%
    collect()

  sprCutoff <- data.frame(type = c('Sprint','Sprint'),
                          gender = c('Men','Women'),
                          yint = c(30,30))

  wjc_med <- wjc %>%
    group_by(nation,gender,type,season) %>%
    summarise(med = median(rank,na.rm = TRUE)) %>%
    mutate(date = season_to_date(season))

  wjc <- split(wjc,wjc$nation)
  wjc_med <- split(wjc_med,wjc_med$nation)

  f_plot <- function(dat1,dat2){
    p <- ggplot(data = dat1,aes(x = as.Date(date),y = rank)) +
      facet_grid(gender~type) +
      geom_hline(data = sprCutoff,aes(yintercept = yint),color = "red") +
      geom_point() +
      geom_line(data = dat2,aes(y = med),color = "blue") +
      labs(x = NULL,y = "Finishing Place")
    return(p)
  }

  plots <- mapply(f_plot,dat1 = wjc,dat2 = wjc_med,USE.NAMES = TRUE,SIMPLIFY = FALSE)

  return(list(plots = plots,
              data = wjc))
}


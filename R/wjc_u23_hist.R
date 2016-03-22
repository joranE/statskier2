#' WJC & U23 History Plots
#'
#' Results trends for nations at either World Juniors or
#' U23s, by gender and race type.
#'
#' @param nations character vector of nations
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
  p <- wjc %>%
    group_by(nation) %>%
    do(plot = ggplot(data = .,aes(x = season,y = rank)) +
         facet_grid(gender~type) +
         geom_hline(data = sprCutoff,aes(yintercept = yint),color = "black") +
         geom_point() +
         stat_summary(fun.y = median,
                      aes(group = 1),
                      geom = "line",
                      color = "blue") +
         labs(x = NULL,y = "Finishing Place") +
         theme(axis.text.x = element_text(angle = 310,hjust = 0,vjust = 1)))
  return(list(plots = setNames(p$plot,p$nation),
              data = wjc))
}


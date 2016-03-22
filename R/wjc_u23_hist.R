#' WJC & U23 History Plots & Data
#'
#' @param nations character vector of nations
#' @param races character; one of "WJC" or "U23"
#' @export
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


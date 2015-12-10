#' @importFrom RColorBrewer brewer.pal
#' @export
spr_plot <- function(ath_names,by_tech = FALSE){
  src <- src_sqlite(path = statskier2::sqlite_path,create = FALSE)

  if (length(ath_names) == 1){
    ath_names <- c(ath_names,ath_names)
  }

  if (by_tech){
    grps <- c('name','season','tech_long','level')
    facet <- facet_grid(name~tech_long)
  }else{
    grps <- c('name','season','level')
    facet <- facet_wrap(~name)
  }

  ath_data <- tbl(src = src,"main") %>%
    filter(cat1 %in% MAJ_INT &
             name %in% ath_names &
             type == 'Sprint' &
             season >= '2005-2006') %>%
    collect() %>%
    mutate(level = cut(rank,
                       breaks = c(-Inf,6,12,30,Inf),
                       labels = c('Final','Semi','Quarter','Qual')),
           tech_long = ifelse(tech == 'F','Freestyle','Classic')) %>%
    group_by_(.dots = grps) %>%
    summarise(n_result = n()) %>%
    mutate(level = factor(level,
                          levels = c('Qual','Final','Semi','Quarter'),
                          labels = paste(c(1,4:2),c('Qual','Final','Semi','Quarter'))))

  lower <- filter(ath_data,level == '1 Qual')
  upper <- filter(ath_data,level != '1 Qual')

  upper$level <- factor(upper$level,levels = rev(levels(upper$level)))

  p <- ggplot() +
    facet +
    geom_bar(data = upper,
             aes(x = season,y = n_result,fill = level,order = level),
             width = 0.5,
             position = "stack",
             stat = "identity") +
    geom_bar(data = lower,
             aes(x = season,y = -n_result,fill = level),
             width = 0.5,
             position = "stack",
             stat = "identity") +
    geom_hline(yintercept = 0,color = "black") +
    labs(x = NULL,y = "Number of Races",fill = "Max\nround\nreached") +
    scale_fill_manual(values = RColorBrewer::brewer.pal(6,"Blues")[3:6],
                      breaks = c('4 Final','3 Semi','2 Quarter','1 Qual')) +
    theme(axis.text.x = element_text(hjust = 0,vjust = 1,angle = 310))


  return(list(plot = p,
              ath_data = ath_data))
}
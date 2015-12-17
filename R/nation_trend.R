#' National Level Performance Trends
#'
#' Only look at major international results.
#'
#' @param nations character vector of nations
#' @param race_gender character; one of "Men" or "Women"
#' @param race_type character; one of "Distance" or "Sprint"
#' @importFrom tidyr gather
#' @export
nation_trend <- function(nations,
                         race_gender = c('Men','Women'),
                         race_type = c('Distance','Sprint')){
  src <- src_sqlite(path = statskier2:::sqlite_path,create = FALSE)

  if (length(nations) == 1){
    nations <- c(nations,nations)
  }

  race_gender <- match.arg(race_gender)
  race_type <- match.arg(race_type)

  nation_data <- tbl(src = src,"main") %>%
    filter(cat1 %in% MAJ_INT &
             nation %in% nations &
             type == race_type &
             gender == race_gender) %>%
    select(season,start,nation,rank) %>%
    collect()

  race_data <- tbl(src = src,"main") %>%
    filter(cat1 %in% MAJ_INT &
             type == race_type &
             gender == race_gender) %>%
    group_by(season) %>%
    summarise(n_races = n_distinct(raceid)) %>%
    collect()

  k <- switch(race_type,'Distance' = 5,'Sprint' = 6)
  k_label <- switch(race_type,'Distance' = 'TopFive','Sprint' = 'TopSix')

  if (race_type == 'Distance'){
    nation_summary <- nation_data %>%
      group_by(season,nation) %>%
      summarise(Wins = sum(rank == 1,na.rm = TRUE),
                Top3 = sum(rank <= 3,na.rm = TRUE),
                Top5 = sum(rank <= 5,na.rm = TRUE),
                Top10 = sum(rank <= 10,na.rm = TRUE),
                Top30 = sum(rank <= 30,na.rm = TRUE)) %>%
      left_join(race_data,by = "season") %>%
      mutate_each(funs(prop = . / n_races),Wins:Top30) %>%
      select(-n_races) %>%
      gather(key = Result,value = Proportion,Wins:Top30) %>%
      mutate(Result = factor(Result,levels = c('Wins','Top3','Top5','Top10','Top30')))
  }else{
    nation_summary <- nation_data %>%
      filter(season >= '2000-2001') %>%
      group_by(season,nation) %>%
      summarise(Wins = sum(rank == 1,na.rm = TRUE),
                Top3 = sum(rank <= 3,na.rm = TRUE),
                Top6 = sum(rank <= 6,na.rm = TRUE),
                Top12 = sum(rank <= 12,na.rm = TRUE),
                Top30 = sum(rank <= 30,na.rm = TRUE)) %>%
      left_join(race_data,by = "season") %>%
      mutate_each(funs(prop = . / n_races),Wins:Top30) %>%
      select(-n_races) %>%
      gather(key = Result,value = Proportion,Wins:Top30) %>%
      mutate(Result = factor(Result,levels = c('Wins','Top3','Top6','Top12','Top30')))
  }

  nation_summary$facet_nation <- paste0(nation_summary$nation," (",paste(race_gender,race_type,sep = ","),")")

  p <- ggplot(nation_summary,aes(x = season,y = Proportion,color = Result)) +
    facet_wrap(~facet_nation) +
    geom_point() +
    geom_line(aes(group = Result)) +
    labs(x = NULL,y = "Results Per Race",color = "") +
    theme(axis.text.x = element_text(hjust = 0,vjust = 1,angle = 310,size = 7),
          legend.position = "bottom",
          legend.direction = "horizontal")

  return(list(plot = p,
              nation_data = nation_data,
              nation_summary = nation_summary,
              race_data = race_data))
}

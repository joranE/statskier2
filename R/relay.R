# library(rvest)
# library(dplyr)
#
# u <- "http://data.fis-ski.com/dynamic/results.html?sector=CC&raceid=25758"
# x <- read_html(u) %>% html_table(fill = TRUE) %>% magrittr::extract2(2)
#
# x <- x[,!sapply(x,function(y) {all(is.na(y))})]
# colnames(x)[2:3] <- c('Team','Name')
# x <- x[,1:6]
# x <- x[!apply(x,1,function(y) {all(is.na(y) | y == '')}),]
#
# x[] <- lapply(x,function(y) {stringr::str_trim(y)})
# x[x == '' | x == '&nbsp'] <- NA
#
# x$Rank <- zoo::na.locf(x$Rank)
# x$Team <- zoo::na.locf(x$Team)
# x$Nation <- zoo::na.locf(x$Nation)
#
# team_results <- filter(x,is.na(Name)) %>%
#   mutate(Time = fiscrape:::convertTime(Time,'Distance')) %>%
#   select(-Name,-Year)
# ind_results <- filter(x,!is.na(Name)) %>%
#   mutate(Time = fiscrape:::convertTime(Time,'Distance')) %>%
#   group_by(Rank,Team) %>%
#   mutate(Leg = 1:n(),
#          Tech = ifelse(Leg <= 2,'C','F')) %>%
#   group_by(Leg) %>%
#   mutate(LegRank = min_rank(Time))

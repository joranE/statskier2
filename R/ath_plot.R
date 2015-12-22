#' Distance Results Plots
#'
#' @param ath_names character vector of athlete names
#' @param races character; either "maj_int" or "fis"
#' @param by_tech boolean; summarise by season or by season and technique
#' @param collapse names list of athletes to collapse into a single panel
#' @export
ath_plot_dst <- function(ath_names,
                         races = c("maj_int","fis"),
                         by_tech = FALSE,
                         collapse = NULL){
  src <- src_sqlite(path = statskier2:::sqlite_path,create = FALSE)

  if (length(ath_names) == 1){
    ath_names <- c(ath_names,ath_names)
  }

  if (length(races) > 1){
    races <- "maj_int"
  }

  ath_data <- tbl(src = src,"main") %>%
    filter(name %in% ath_names &
             type == 'Distance') %>%
    collect() %>%
    mutate(tech_name = ifelse(tech == 'C','Classic',
                              ifelse(tech == 'F','Freestyle','Pursuit')))

  if (races == "maj_int"){
    ath_data <- filter(ath_data,cat1 %in% MAJ_INT) %>%
      mpb() %>%
      standardize_mpb()
  }

  if (!is.null(collapse)){
    ath_data$name[ath_data$name %in% collapse] <- names(collapse)
  }

  if (races == "maj_int"){
    ath_data$y <- ath_data$mpb
    ylab <- "Standardized % Back From Median Skier"
  }else{
    ath_data$y <- ath_data$fispoints
    ylab <- "FIS Points"
  }

  if (by_tech){
    grps <- c("name","season","tech_name")
  }else{
    grps <- c("name","season")
  }

  ath_summary <- ath_data %>%
    group_by_(.dots = grps) %>%
    summarise(med = median(y,na.rm = TRUE),
              n = n()) %>%
    as.data.frame() %>%
    filter(n >= 2) %>%
    mutate(date = paste0(substr(season,6,9),"-01-01"))

  ath_data$date <- as.Date(ath_data$date)
  ath_summary$date <- as.Date(ath_summary$date)

  ath_data$name <- factor(ath_data$name,
                          levels = unique(ath_names))
  ath_summary$name <- factor(ath_summary$name,
                             levels = unique(ath_names))

  if (by_tech){
    line_piece <- geom_line(data = ath_summary,
                            aes(x = date,y = med,color = tech_name))
  }else{
    line_piece <- geom_line(data = ath_summary,
                            aes(x = date,y = med,group = name),color = "blue")
  }

  p <- ggplot() +
    facet_wrap(~name) +
    geom_point(data = ath_data,aes(x = date,y = y),alpha = 0.5) +
    line_piece +
    labs(x = NULL,
         y = ylab,
         color = "") +
    theme(axis.text.x = element_text(angle = 310,hjust = 0,vjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal")

  return(list(plot = p,
              ath_data = ath_data,
              ath_summary = ath_summary))
}

#' Sprint Results Plots
#'
#' @param ath_names character vector of athlete names
#' @param races character; either "maj_int" or "fis"
#' @param by_tech boolean; summarise by season or by season and technique
#' @param collapse names list of athletes to collapse into a single panel
#' @export
ath_plot_spr <- function(ath_names,
                         races = c("maj_int","fis"),
                         by_tech = FALSE,
                         collapse = NULL){
  src <- src_sqlite(path = statskier2:::sqlite_path,create = FALSE)

  if (length(ath_names) == 1){
    ath_names <- c(ath_names,ath_names)
  }

  if (length(races) > 1){
    races <- "maj_int"
  }

  ath_data <- tbl(src = src,"main") %>%
    filter(name %in% ath_names &
             type == 'Sprint') %>%
    collect() %>%
    mutate(tech_name = ifelse(tech == 'C','Classic','Freestyle'))

  if (races == "maj_int"){
    ath_data <- filter(ath_data,cat1 %in% MAJ_INT)
  }

  if (!is.null(collapse)){
    ath_data$name[ath_data$name %in% collapse] <- names(collapse)
  }

  if (by_tech){
    grps <- c("name","season","tech_name")
  }else{
    grps <- c("name","season")
  }

  ath_summary <- ath_data %>%
    group_by_(.dots = grps) %>%
    summarise(med = median(rank,na.rm = TRUE),
              n = n()) %>%
    as.data.frame() %>%
    filter(n >= 2) %>%
    mutate(date = paste0(substr(season,6,9),"-01-01"))

  ath_data$date <- as.Date(ath_data$date)
  ath_summary$date <- as.Date(ath_summary$date)

  ath_data$name <- factor(ath_data$name,
                          levels = unique(ath_names))
  ath_summary$name <- factor(ath_summary$name,
                          levels = unique(ath_names))

  if (by_tech){
    line_piece <- geom_line(data = ath_summary,
                            aes(x = date,y = med,color = tech_name))
  }else{
    line_piece <- geom_line(data = ath_summary,
                            aes(x = date,y = med,group = name),color = "blue")
  }

  p <- ggplot() +
    facet_wrap(~name) +
    geom_hline(yintercept = 30,color = "red") +
    geom_point(data = ath_data,aes(x = date,y = rank),alpha = 0.5) +
    line_piece +
    labs(x = NULL,
         y = "Finishing Place",
         color = "") +
    theme(axis.text.x = element_text(angle = 310,hjust = 0,vjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal")

  return(list(plot = p,
              ath_data = ath_data,
              ath_summary = ath_summary))

}

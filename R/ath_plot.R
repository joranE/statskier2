#' Sprint & Distance Results Plots By Athlete
#'
#' Faceted plots of results over time for one or several athletes using
#' either FIS points, median percent back or finishing place as
#' a measure of performance. Includes summaries by season or by season and
#' technique.
#'
#' @param ath_names character vector of athlete names
#' @param races character; either "maj_int" for only major international
#' results or "fis" for all results
#' @param by_tech boolean; if \code{TRUE} summarise by both season and technique,
#' rather than just season
#' @param collapse named list of athletes to collapse into a single panel
#' @param use_rank boolean; if \code{TRUE} use rank (finishing place) instead
#' of median percent back or FIS points when races = "maj_int".
#' @return List with three components,
#'  \enumerate{
#'    \item \code{plot} - ggplot2 plot object
#'    \item \code{ath_data} - raw athlete data
#'    \item \code{ath_summary} - athlete data summarised by season/technique
#'  }
#' @importFrom dbplyr in_schema
#' @export
#' @examples
#' \dontrun{
#' p <- ath_plot_dst(ath_names = c('DIGGINS Jessica','MAUBET BJORNSEN Sadie','CALDWELL Sophie'),
#'                   races = "maj_int")
#' print(p$plot)
#' }
ath_plot_dst <- function(ath_names,
                         races = c("maj_int","fis"),
                         by_tech = FALSE,
                         collapse = NULL,
                         use_rank = FALSE){

  if (length(ath_names) == 1){
    ath_names <- c(ath_names,ath_names)
  }

  if (length(races) > 1){
    races <- "maj_int"
  }

  ath_data <- tbl(src = ..statskier_pg_con..,
                  dbplyr::in_schema("public","v_distance")) %>%
    filter(name %in% ath_names) %>%
    collect() %>%
    mutate_if(.predicate = bit64::is.integer64,.funs = as.integer) %>%
    mutate(tech_name = ifelse(tech == 'C','Classic',
                              ifelse(tech == 'F','Freestyle','Pursuit')))

  if (races == "maj_int"){
    ath_data <- filter(ath_data,primary_tag %in% MAJ_INT)
  }

  if (!is.null(collapse)){
    for (i in seq_along(collapse)){
      ath_data$name[ath_data$name %in% collapse[[i]]] <- names(collapse)[i]
    }
  }

  if (races == "maj_int"){
    if (use_rank){
      ath_data$y <- ath_data$rank
      ylab <- "Finishing Place"
    }else{
      ath_data$y <- ath_data$pbm
      ylab <- "% Back From Median Skier"
    }
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
    group_by_at(.vars = grps) %>%
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
                            aes(x = date,
                                y = med,
                                color = tech_name))
  }else{
    line_piece <- geom_line(data = ath_summary,
                            aes(x = date,
                                y = med,
                                group = name),
                            color = "blue")
  }

  p <- ggplot() +
    facet_wrap(~name) +
    geom_point(data = ath_data,
               aes(x = date,y = y),
               alpha = 0.5) +
    line_piece +
    labs(x = NULL,
         y = ylab,
         color = "") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 310,
                                     hjust = 0,
                                     vjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal")

  return(list(plot = p,
              ath_data = ath_data,
              ath_summary = ath_summary))
}

#' @rdname ath_plot_dst
#' @export
ath_plot_spr <- function(ath_names,
                         races = c("maj_int","fis"),
                         by_tech = FALSE,
                         collapse = NULL,
                         use_rank = TRUE){

  if (length(ath_names) == 1){
    ath_names <- c(ath_names,ath_names)
  }

  if (length(races) > 1){
    races <- "maj_int"
  }

  ath_data <- tbl(src = ..statskier_pg_con..,
                  dbplyr::in_schema("public","v_sprint")) %>%
    filter(name %in% ath_names) %>%
    collect() %>%
    mutate_if(.predicate = bit64::is.integer64,.funs = as.integer) %>%
    mutate(tech_name = ifelse(tech == 'C','Classic','Freestyle'))

  if (races == "maj_int"){
    ath_data <- filter(ath_data,primary_tag %in% MAJ_INT)
  }

  if (!is.null(collapse)){
    for (i in seq_along(collapse)){
      ath_data$name[ath_data$name %in% collapse[[i]]] <- names(collapse)[i]
    }
  }

  if (by_tech){
    grps <- c("name","season","tech_name")
  }else{
    grps <- c("name","season")
  }

  if (use_rank){
    ath_data$y <- ath_data$rank
    ylab <- "Finishing Place"
  }else{
    ath_data$y <- ath_data$fispoints
    ylab <- "FIS Points"
  }

  ath_summary <- ath_data %>%
    group_by_at(.vars = grps) %>%
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
                            aes(x = date,
                                y = med,
                                color = tech_name))
  }else{
    line_piece <- geom_line(data = ath_summary,
                            aes(x = date,
                                y = med,
                                group = name),
                            color = "blue")
  }

  p <- ggplot() +
    facet_wrap(~name) +
    geom_hline(yintercept = 30,color = "red") +
    geom_point(data = ath_data,
               aes(x = date,y = y),
               alpha = 0.5) +
    line_piece +
    labs(x = NULL,
         y = ylab,
         color = "") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 310,
                                     hjust = 0,
                                     vjust = 1),
          legend.position = "bottom",
          legend.direction = "horizontal")

  return(list(plot = p,
              ath_data = ath_data,
              ath_summary = ath_summary))

}

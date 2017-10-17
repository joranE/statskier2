#' importFrom tidyr gather
#' @export
ggbump <- function(grp_var,data,offset.x = 0.1,xlab = NULL,yrev = TRUE,title = NULL,subtitle = NULL){
  idx <- which(grp_var == names(data))
  names(data)[idx] <- ".group"

  data_gather <- gather(data,key = "x",value = "y",-.group) %>%
    arrange(.group,x,y)

  do_embed <- function(x,dim = 2,var){
    setNames(as.data.frame(embed(x,dim)[,2:1,drop = FALSE]),paste0(var,1:2))
  }

  x_levs <- as.character(sort(unique(data_gather$x)))

  a <- data_gather %>%
    group_by(.group) %>%
    do(X = do_embed(.$x,2,"x"),
       Y = do_embed(.$y,2,"y"))

  b <- unnest(a,X,Y) %>%
    mutate(x1 = factor(x1,levels = x_levs),
           x2 = factor(x2,levels = x_levs),
           x1_p = as.integer(x1),
           x2_p = as.integer(x2))
  text_df <- bind_rows(
    b %>% select(.group,x1_p,y1),
    b %>% filter(x2_p == max(x2_p)) %>% select(.group,x2_p,y2) %>% rename(x1_p = x2_p,y1 = y2)
  ) %>%
    distinct()


  labs_left <-   b %>%
    filter(x1_p == min(x1_p) & !is.na(y1)) %>%
    select(.group,x = x1_p,y = y1)
  labs_right <- b %>%
    filter(x2_p == max(x2_p) & !is.na(y2)) %>%
    select(.group,x = x2_p,y = y2)

  p <- ggplot() +
    geom_segment(data = b,aes(x = x1_p + offset.x,
                              y = y1,
                              xend = x2_p - offset.x,
                              yend = y2)) +
    geom_text(data = text_df,
              aes(x = x1_p,y = y1,label = as.character(y1)),
              size = 2) +
    geom_text(data = labs_left,
              aes(x = x - offset.x,y = y,label = .group),
              size = 2.5,
              hjust = 1) +
    geom_text(data = labs_right,
              aes(x = x + offset.x,y = y,label = .group),
              size = 2.5,
              hjust = 0) +
    scale_x_continuous(breaks = c(0,seq_along(x_levs),length(x_levs)+1),
                       labels = c("",x_levs,"")) +
    coord_cartesian(c(0,length(x_levs) + 1)) +
    labs(y = NULL,x = xlab) +
    ggtitle(label = title,subtitle = subtitle) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

  if (yrev) p <- p + scale_y_reverse()
  p
}

#' Sprint Heat Round Plot
#'
#' @importFrom egg ggarrange
#' @export
spr_heat_plot <- function(.eventid,subtitle = ""){

  race_info <- tbl(conl,"v_sprint_heats") %>%
    filter(eventid == .eventid) %>%
    select(eventid,date,season,location,tech,length,gender) %>%
    distinct() %>%
    collect()

  spr_qual <- tbl(conl,"v_sprint") %>%
    filter(eventid == .eventid) %>%
    collect() %>%
    select(eventid,name,nation,time,rank = rankqual) %>%
    mutate(heat = "qual",
           heat = factor(heat,
                         levels = c("qual","quarter","semi","final"),
                         labels = c("Qual","QF","SF","Final")),
           qf = NA_integer_,
           sf = NA_integer_,
           fn = NA_integer_)
  spr_heats <- tbl(conl,"v_sprint_heats") %>%
    filter(eventid == .eventid) %>%
    collect() %>%
    select(eventid,name,nation,time = heat_time,rank = heat_rank,qf,sf,fn,heat) %>%
    mutate(heat = case_when(substr(heat,1,1) == "1" ~ "quarter",
                            substr(heat,1,1) == "2" ~ "semi",
                            substr(heat,1,1) == "3" ~ "final"),
           heat = factor(heat,
                         levels = c("qual","quarter","semi","final"),
                         labels = c("Qual","QF","SF","Final")))

  lls <- tbl(conl,"v_sprint_heats") %>%
    filter(eventid == .eventid) %>%
    collect() %>%
    filter(ll == "Y") %>%
    select(name,qf,sf) %>%
    tidyr::pivot_longer(cols = c("qf","sf"),names_to = "heat") %>%
    filter(!is.na(value)) %>%
    mutate(heat = toupper(heat)) %>%
    group_by(name) %>%
    summarise(ll_heat = paste(heat,collapse = ",")) %>%
    mutate(ll_name = paste(name,paste0("(LL: ",ll_heat,")"))) %>%
    select(name,ll_name)

  data_clean <- bind_rows(spr_qual,spr_heats) %>%
    mutate(heat_lab = as.character(coalesce(qf,sf)),
           heat_lab = if_else(heat == "Qual",NA_character_,heat_lab))
  data_clean <- left_join(data_clean,
                          lls,by = "name") %>%
    mutate(name = coalesce(ll_name,name))
  spr_final <- data_clean %>%
    filter(name %in% data_clean$name[data_clean$fn == 1])
  name_lev_ord <- spr_final %>%
    filter(heat == "Final") %>%
    arrange(rank) %>%
    pull(name)
  spr_final$name <- factor(spr_final$name,levels = name_lev_ord)

  title <- paste(race_info$date,race_info$location,race_info$gender,paste0(race_info$length,"km"),race_info$tech)

  data_clean <- data_clean %>%
    group_by(name) %>%
    mutate(qual_only = all(heat == "Qual")) %>%
    ungroup() %>%
    filter(!qual_only)

  p <- ggplot(data = data_clean,aes(x = heat,y = time,group = name)) +
    geom_line(alpha = 0.5) +
    geom_line(data = spr_final,aes(color = name,group = name),size = 1.1) +
    geom_text(aes(label = heat_lab),hjust = rep(c(1,0),length.out = nrow(data_clean))) +
    scale_color_brewer(palette = "Set2") +
    labs(x = "Round",y = "Time (seconds)",color = NULL,caption = "@statskier - statisticalskier.com") +
    ggtitle(label = title,subtitle = subtitle) +
    theme_bw()
  p
}

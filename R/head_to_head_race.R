#' @export
hth_race <- function(ath1_name,
                     race_id,
                     num_ath2,
                     cutoff = 365 * 5,
                     min_encounters = 1,
                     measure = c('rank','fispoints','pb'),
                     restrict_by = NULL){
  src <- src_sqlite(path = statskier2:::sqlite_path,create = FALSE)

  measure <- match.arg(measure)

  race_data <- tbl(src = src,"main") %>%
    filter(raceid == race_id &
             rank <= num_ath2) %>%
    collect()

  ath2_names <- select(race_data,fisid,name) %>%
    filter(name != ath1_name)

  race_info <- race_data %>%
    select(raceid,type,tech,start,length) %>%
    unique()

  if (race_info$type == 'Distance'){
    hth_data <- hth_dst(ath1 = ath1_name,
                        ath2 = ath2_names$name,
                        races = "fis",
                        measure = measure)$data %>%
      group_by(facet_name) %>%
      mutate(n_races = n_distinct(raceid)) %>%
      ungroup() %>%
      filter(n_races >= min_encounters) %>%
      as.data.frame()
  }
  if(race_info$type == 'Sprint'){
    hth_data <- hth_spr(ath1 = ath1_name,
                        ath2 = ath2_names,
                        races = "fis",
                        measure = measure)$data
  }

  return(list(data = hth_data))
}

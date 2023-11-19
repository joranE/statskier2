#' @importFrom glue glue_sql
#' @export
dst_hth_data <- function(ath_compid,opp_compid){
  if (length(ath_compid) == 0 || length(ath_compid) > 1){
    stop("Length of 'ath_compid' must be at least 1.")
  }
  if (length(opp_compid) < 1){
    stop("Length of 'opp_compid' must be at least 1.")
  }
  sql <- read_sql("inst/sql/skier_head_to_head_dst.sql")
  sql <- glue::glue_sql(sql,
                        ath_compid = ath_compid,
                        opp_compid = opp_compid,
                        .con = ..statskier_pg_con..)
  result <- RPostgres::dbGetQuery(..statskier_pg_con..,sql) |>
    dplyr::mutate_if(.predicate = bit64::is.integer64,.funs = as.integer)
  result
}

#' @export
spr_hth_data <- function(ath_compid,opp_compid){
  if (length(ath_compid) == 0 || length(ath_compid) > 1){
    stop("Length of 'ath_compid' must be at least 1.")
  }
  if (length(opp_compid) < 1){
    stop("Length of 'opp_compid' must be at least 1.")
  }
  sql <- read_sql("inst/sql/skier_head_to_head_spr.sql")
  sql <- glue::glue_sql(sql,
                        ath_compid = ath_compid,
                        opp_compid = opp_compid,
                        .con = ..statskier_pg_con..)
  result <- RPostgres::dbGetQuery(..statskier_pg_con..,sql) |>
    dplyr::mutate_if(.predicate = bit64::is.integer64,.funs = as.integer)
  result
}

#' @export
spr_heats_hth_data <- function(ath_compid,opp_compid){
  if (length(ath_compid) == 0 || length(ath_compid) > 1){
    stop("Length of 'ath_compid' must be at least 1.")
  }
  if (length(opp_compid) < 1){
    stop("Length of 'opp_compid' must be at least 1.")
  }
  sql <- read_sql("inst/sql/skier_head_to_head_spr_heats.sql")
  sql <- glue::glue_sql(sql,
                        ath_compid = ath_compid,
                        opp_compid = opp_compid,
                        .con = ..statskier_pg_con..)
  result <- RPostgres::dbGetQuery(..statskier_pg_con..,sql) |>
    dplyr::mutate_if(.predicate = bit64::is.integer64,.funs = as.integer)
  result
}

#' @export
common_opponents <- function(ath_compid1,ath_compid2,min_date,max_date){
  if (length(ath_compid1) != 1){
    stop("Length of 'ath_compid1' must be 1.")
  }
  if (length(ath_compid2) != 1){
    stop("Length of 'ath_compid2' must be 1.")
  }
  sql <- read_sql("inst/sql/common_opponents_dst.sql")
  sql <- glue::glue_sql(sql,
                        skier1 = ath_compid1,
                        skier2 = ath_compid2,
                        min_date = min_date,
                        max_date = max_date,
                        .con = ..statskier_pg_con..)
  result <- RPostgres::dbGetQuery(..statskier_pg_con..,sql) |>
    dplyr::mutate_if(.predicate = bit64::is.integer64,.funs = as.integer)
  result
}

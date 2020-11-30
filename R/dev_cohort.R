#' importFrom qgam mqgam qgam
#' @export
dev_cohort_dst_qgam <- function(top_k = 10,
                                times = 3,
                                q = c(0.1,0.9),
                                type = "fispoints"){
  dst_sql <- statskier2::read_sql("inst/sql/cohort_dst.sql")
  dst_sql <- glue::glue_sql(dst_sql,
                            rnk = top_k,
                            topn = times,
                            .con = ..statskier_pg_con..)
  message("Fetching data...")
  dst_result <- RPostgres::dbGetQuery(..statskier_pg_con..,dst_sql)
  dst_result$gender <- as.factor(dst_result$gender)
  dst_result <- dst_result %>%
    mutate_if(.predicate = bit64::is.integer64,.funs = as.integer)

  if (type == "fispoints"){
    dst_result <- dst_result %>%
      filter(!is.na(fispoints) & !is.na(age))
    dst_result_men <- filter(dst_result,gender == "Men")
    dst_result_wom <- filter(dst_result,gender == "Women")
    tuning_data_men <- dst_result_men %>%
      slice_sample(prop = 0.1)
    tuning_data_wom <- dst_result_wom %>%
      slice_sample(prop = 0.1)
    message("Estimating lsig...")
    tune_lsig_men <- qgam::tuneLearnFast(form = fispoints ~ s(age),
                                     data = tuning_data_men,
                                     qu = q,
                                     control = list(progress = FALSE))
    tune_lsig_wom <- qgam::tuneLearnFast(form = fispoints ~ s(age),
                                         data = tuning_data_wom,
                                         qu = q,
                                         control = list(progress = FALSE))

    message("Fitting full model...")
    dst_mod_men <- qgam::mqgam(form = fispoints ~ s(age),
                           data = dst_result_men,
                           qu = q,
                           lsig = tune_lsig_men$lsig)
    dst_mod_wom <- qgam::mqgam(form = fispoints ~ s(age),
                               data = dst_result_wom,
                               qu = q,
                               lsig = tune_lsig_wom$lsig)
  }
  if (type == "pbm_pts"){
    dst_result <- dst_result %>%
      filter(!is.na(pbm_pts) & !is.na(age))
    tuning_data <- dst_result %>%
      slice_sample(prop = 0.1)
    message("Estimating lsig...")
    tune_lsig <- qgam::tuneLearnFast(form = pbm_pts ~ s(age,by = gender),
                                     data = tuning_data,
                                     qu = q,
                                     control = list(progress = FALSE))
    message("Fitting full model...")
    dst_mod <- qgam::mqgam(form = pbm_pts ~ s(age,by = gender),
                           data = dst_result,
                           qu = q,
                           lsig = tune_lsig$lsig)
  }

  message("Creating fitted values...")
  # new_data_dst <- tidyr::crossing(gender = c("Men","Women"),
  #                                 age = seq(18,45,by = 0.5))
  new_data_dst_men <- data.frame(age = seq(18,45,by = 0.5),
                                 gender = "Men")
  new_data_dst_wom <- data.frame(age = seq(18,45,by = 0.5),
                                 gender = "Women")
  new_data_dst_men$q_upper <- qgam::qdo(dst_mod_men,qu = q[2],
                                    predict,
                                    newdata = new_data_dst_men)
  new_data_dst_men$q_lower <- qgam::qdo(dst_mod_men,qu = q[1],
                                    predict,
                                    newdata = new_data_dst_men)

  new_data_dst_wom$q_upper <- qgam::qdo(dst_mod_wom,qu = q[2],
                                        predict,
                                        newdata = new_data_dst_wom)
  new_data_dst_wom$q_lower <- qgam::qdo(dst_mod_wom,qu = q[1],
                                        predict,
                                        newdata = new_data_dst_wom)

  bind_rows(new_data_dst_men,new_data_dst_wom)
}

#' @export
dev_cohort_spr_qgam <- function(top_k = 12,
                                times = 3,
                                q = c(0.1,0.9),
                                type = "fispoints"){
  spr_sql <- squr::read_sql("inst/sql/cohort_spr.sql")
  spr_sql <- glue::glue_sql(spr_sql,
                            rnk = top_k,
                            topn = times,
                            .con = ..statskier_pg_con..)
  spr_result <- RPostgres::dbGetQuery(..statskier_pg_con..,spr_sql)
  spr_result$gender <- as.factor(spr_result$gender)
  spr_result <- spr_result %>%
    mutate_if(.predicate = bit64::is.integer64,.funs = as.integer)

  if (type == "fispoints"){
    spr_result <- spr_result %>%
      filter(!is.na(fispoints) & !is.na(age))
    spr_mod <- qgam::mqgam(form = fispoints ~ s(age,by = gender),
                           data = spr_result,
                           qu = q,
                           lsig = c(2.42,3.00))
  }
  if (type == "pbm_pts"){
    spr_result <- spr_result %>%
      filter(!is.na(pbm_pts) & !is.na(age))
    spr_mod <- qgam::mqgam(form = pbm_pts ~ s(age,by = gender),
                           data = spr_result,
                           qu = q,
                           lsig = c(2.42,3.00))
  }


  new_data_spr <- tidyr::crossing(gender = c("Men","Women"),age = seq(18,45,by = 0.5))
  new_data_spr$q_upper <- qgam::qdo(spr_mod,qu = q[2],predict,newdata = new_data_spr)
  new_data_spr$q_lower <- qgam::qdo(spr_mod,qu = q[1],predict,newdata = new_data_spr)

  new_data_spr
}

#' @export
dev_cohort_dst <- function(ath_fisid,
                           cohort_data,
                           type = "fispoints",
                           .label = "Development Trends",
                           .subtitle = ""){
  ath_res <- skier_results(.fisid = ath_fisid)

  ath_res$dst <- ath_res$dst %>%
    mutate(fisid_fac = factor(fisid,levels = ath_fisid)) %>%
    arrange(fisid_fac,date) %>%
    mutate(name_fac = forcats::fct_inorder(name)) %>%
    mutate_if(.predicate = bit64::is.integer64,.funs = as.integer)

  max_age_dst <- max(ath_res$dst$age,na.rm = TRUE) + 1

  new_data_dst_name <- ath_res$dst %>%
    select(gender,name) %>%
    distinct() %>%
    left_join(cohort_data,by = "gender")

  ylab <- switch(EXPR = type,fispoints = "FIS Points",pbm_pts = "PBM Points")

  p_dst <- ggplot(data = ath_res$dst,aes(x = age,y = .data[[type]])) +
    facet_wrap(~name_fac) +
    geom_ribbon(data = new_data_dst_name,
                aes(y = NULL,ymin = q_lower,ymax = q_upper),
                fill = "blue",alpha = 0.2) +
    geom_point() +
    geom_smooth(formula = y~s(x),
                method = qgam_,
                method.args = list(qu = c(0.1)),
                se = FALSE,color = "red") +
    geom_smooth(formula = y~s(x),
                method = qgam_,
                method.args = list(qu = c(0.9)),
                se = FALSE,color = "red") +
    coord_cartesian(xlim = c(15,max_age_dst)) +
    labs(x = "Age",y = ylab,caption = "@statskier - statisticalskier.com") +
    ggtitle(label = .label,subtitle = .subtitle) +
    theme_bw()

  list(dst = p_dst,ath_data = ath_res$dst,cohort_data = new_data_dst_name)
}

#' @export
dev_cohort_spr <- function(ath_fisid,
                           cohort_data,
                           type = "fispoints",
                           .label = "Development Trends",
                           .subtitle = ""){
  ath_res <- skier_results(.fisid = ath_fisid)

  ath_res$spr <- ath_res$spr %>%
    mutate(fisid_fac = factor(fisid,levels = ath_fisid)) %>%
    arrange(fisid_fac,date) %>%
    mutate(name_fac = forcats::fct_inorder(name)) %>%
    mutate_if(.predicate = bit64::is.integer64,.funs = as.integer)

  max_age_spr <- max(ath_res$spr$age,na.rm = TRUE) + 5

  new_data_spr_name <- ath_res$spr %>%
    select(gender,name) %>%
    distinct() %>%
    left_join(cohort_data,by = "gender")

  ylab <- switch(EXPR = type,fispoints = "FIS Points",pbm_pts = "PBM Points")

  p_spr <- ggplot(data = ath_res$spr,aes(x = age,y = .data[[type]])) +
    facet_wrap(~name_fac) +
    geom_ribbon(data = new_data_spr_name,
                aes(y = NULL,ymin = q_lower,ymax = q_upper),
                fill = "blue",alpha = 0.2) +
    geom_point() +
    geom_smooth(formula = y~s(x),
                method = qgam_,
                method.args = list(qu = c(0.5)),
                se = FALSE,color = "red") +
    coord_cartesian(xlim = c(15,max_age_spr)) +
    labs(x = "Age",y = ylab,caption = "@statskier - statisticalskier.com") +
    ggtitle(label = .label,subtitle = .subtitle) +
    theme_bw()

  list(spr = p_spr,ath_data = ath_res$spr,cohort_data = new_data_spr_name)
}

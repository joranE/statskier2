#' Head-to-head Plots & Data
#'
#' Sprint and distance head-to-head plots and accompanying
#' data. Use these to compare some athletes (\code{ath1}) to a
#' collection of other athletes (\code{ath2}).
#'
#' @param ath1 character; single athlete name of interest
#' @param ath2 character vector of athlete names to compare with
#' @param races character; one of "maj_int" (major international races) or "fis"
#' (all FIS races).
#' @param measure character; one of "fispoints", "rank", "pb".
#' @param by_tech boolean; split trend lines by technique. Only one of
#' \code{by_tech} and \code{by_start} can by \code{TRUE}.
#' @param by_start boolean; split trend lines by race type. Only one of
#' \code{by_tech} and \code{by_start} can by \code{TRUE}. Ignored for
#' sprint race comparisons
#' @return A named list with components:
#' \enumerate{
#'  \item \code{plot} - ggplot2 plot object
#'  \item \code{data} - raw head-to-head results data
#'  \item \code{line_data} - summarised head-to-head data for plotted lines
#'  \item \code{block} - data useful only for constructing the plot
#' }
#' @importFrom rlang syms
#' @export
#' @examples
#' \dontrun{
#' p <- hth_dst(
#'   ath1 = "DIGGINS Jessica",
#'   ath2 = c("RANDALL Kikkan", "BJORNSEN Sadie", "SARGENT Ida"),
#'   races = "fis",
#'   measure = "pb"
#' )
#' print(p$plot)
#' }
hth_dst <- function(ath1,
                    ath2,
                    races = c("maj_int", "fis"),
                    measure = c("fispoints", "rank", "pb"),
                    by_tech = FALSE,
                    by_start = FALSE) {
  if (any(ath1 %in% ath2)) {
    ath2 <- setdiff(ath2, ath1)
    warning("Removing ath1 from ath2 names.")
  }

  races <- match.arg(races)
  measure <- match.arg(measure)

  if (by_tech && by_start) {
    stop("Can only group by one of technique or start type.")
  }

  hth_df <- hth_data(
    athletes = ath1,
    opponents = ath2
  ) |>
    filter(type == "Distance") |>
    mutate(start = if_else(start == "Pursuit", "Mass", start))

  if (races == "maj_int") {
    hth_df <- filter(hth_df, cat1 %in% c("WC", "TDS", "OWG", "WSC"))
  }

  hth_df[["y"]] <- hth_df[[paste0("diff_", measure)]]

  summary_grps <- c("ath_name", "opp_name", "season")
  if (by_tech) {
    summary_grps <- c(summary_grps, "tech")
    ath_summary <- hth_df |>
      group_by(!!!rlang::syms(summary_grps)) |>
      summarise(med = median(y, na.rm = TRUE)) |>
      mutate(date = season_to_date(season)) |>
      as.data.frame()
    line_piece <- geom_line(
      data = ath_summary,
      aes(x = as.Date(date), y = med, color = tech),
      size = 1.1
    )
  }
  if (by_start) {
    summary_grps <- c(summary_grps, "start")
    ath_summary <- hth_df |>
      group_by(!!!rlang::syms(summary_grps)) |>
      summarise(med = median(y, na.rm = TRUE)) |>
      mutate(date = season_to_date(season)) |>
      as.data.frame()
    line_piece <- geom_line(
      data = ath_summary,
      aes(x = as.Date(date), y = med, color = start),
      size = 1.1
    )
  }
  if (!by_start && !by_tech) {
    ath_summary <- hth_df |>
      group_by(!!!rlang::syms(summary_grps)) |>
      summarise(med = median(y, na.rm = TRUE)) |>
      mutate(date = season_to_date(season)) |>
      as.data.frame()
    line_piece <- geom_line(
      data = ath_summary,
      aes(x = as.Date(date), y = med),
      color = "blue",
      size = 1.1
    )
  }

  color_label <- c("Technique", "Race Type", "")[c(by_tech, by_start, (!by_tech & !by_start))]
  y_label <- switch(measure,
    "fispoints" = "Difference in FIS Points",
    "rank" = "Difference in Finishing Place",
    "pb" = "Difference in % Back"
  )

  p <- ggplot() +
    facet_grid(ath_name ~ opp_name) +
    geom_hline(yintercept = 0, color = "red") +
    geom_point(data = hth_df, aes(x = as.Date(date), y = y)) +
    line_piece +
    labs(
      x = NULL,
      y = y_label,
      fill = "",
      color = color_label
    ) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    ) +
    guides(fill = guide_legend(override.aes = list(alpha = 0.5)))

  return(list(
    plot = p,
    data = hth_df,
    line_data = ath_summary
  ))
}

#' @rdname hth_dst
#' @export
hth_spr <- function(ath1,
                    ath2,
                    races = c("maj_int", "fis"),
                    measure = c("fispoints", "rank"),
                    by_tech = FALSE,
                    by_start = FALSE) {
  races <- match.arg(races)
  measure <- match.arg(measure)

  hth_df <- hth_data(
    athletes = ath1,
    opponents = ath2
  ) |>
    filter(type == "Sprint")

  if (races == "maj_int") {
    hth_df <- filter(hth_df, cat1 %in% c("WC", "TDS", "OWG", "WSC"))
  }

  hth_df[["y"]] <- hth_df[[paste0("diff_", measure)]]

  summary_grps <- c("ath_name", "opp_name", "season")
  if (by_tech) {
    summary_grps <- c(summary_grps, "tech")
    ath_summary <- hth_df |>
      group_by(!!!rlang::syms(summary_grps)) |>
      summarise(med = median(y, na.rm = TRUE)) |>
      mutate(date = season_to_date(season)) |>
      as.data.frame()
    line_piece <- geom_line(
      data = ath_summary,
      aes(x = as.Date(date), y = med, color = tech),
      size = 1.1
    )
  } else {
    ath_summary <- hth_df |>
      group_by(!!!rlang::syms(summary_grps)) |>
      summarise(med = median(y, na.rm = TRUE)) |>
      mutate(date = season_to_date(season)) |>
      as.data.frame()
    line_piece <- geom_line(
      data = ath_summary,
      aes(x = as.Date(date), y = med),
      color = "blue",
      size = 1.1
    )
  }

  color_label <- c("Technique", "")[c(by_tech, !by_tech)]

  p <- ggplot() +
    facet_wrap(ath_name ~ opp_name) +
    geom_hline(yintercept = 0, color = "red") +
    geom_point(data = hth_df, aes(x = as.Date(date), y = y)) +
    line_piece +
    labs(
      x = NULL,
      y = NULL,
      fill = "",
      color = color_label
    ) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    ) +
    guides(fill = guide_legend(override.aes = list(alpha = 0.5)))

  return(list(
    plot = p,
    data = hth_df,
    line_data = ath_summary
  ))
}

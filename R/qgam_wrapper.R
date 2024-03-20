#' @importFrom qgam qgam
#' @export
qgam_ <- function(formula, data, weights, ...) {
  args <- list(...)
  message("Fitting qgam quantile regression where q=", args$qu)
  qgam::qgam(form = formula, data = data, control = list(progress = FALSE), ...)
}

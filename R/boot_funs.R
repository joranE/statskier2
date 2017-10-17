#' Bootstrap Average of Top n Values
#'
#' Bootstrap the mean of the best (lowest) n values in a vector.
#'
#' @param vec vector of values
#' @param n integer; select n best (lowest) values
#' @param B integer; number of bootstrap replicates
#' @return A list with components \code{orig} with the original statistic and
#' \code{bs} a vector of the bootstrapped statistics
#' @export
bs_top_n <- function(vec,n = 5,B = 1000){
  n_vec <- length(vec)
  vec <- vec[!is.na(vec)]
  if (n_vec < n) return(list(orig = NA,bs = rep(NA,B)))
  orig <- mean(sort(vec)[seq_len(n)])
  mat <- replicate(n = B,sort(sample(x = vec,size = n_vec,replace = TRUE))[seq_len(n)])
  bs_vals <- colMeans(mat)
  list(orig = orig,bs = bs_vals)
}

#' Bootstrap Median
#'
#' Bootstrap the median of a vector.
#'
#' @param vec vector of values
#' @param B integer; number of bootstrap replicates
#' @return A list with components \code{orig} with the original statistic and
#' \code{bs} a vector of the bootstrapped statistics
#' @importFrom matrixStats colMedians
#' @export
bs_median <- function(vec,B = 1000){
  n <- length(vec)
  vec <- vec[!is.na(vec)]
  if (n == 1) return(list(orig = NA,bs = rep(NA,B)))
  orig <- median(vec)
  mat <- matrix(vec[sample(n,n*B,replace = TRUE)],nrow = n,ncol = B,byrow = FALSE)
  #mat <- replicate(n = B,sample(x = vec,size = length(vec),replace = TRUE))
  bs_vals <- matrixStats::colMedians(mat)
  list(orig = orig,bs = bs_vals)
}

#' Bootstrap Quantiles
#'
#' Bootstrap quantiles of a vector.
#'
#' @param vec vector of values
#' @param probs vector of values between 0 and 1; quantiles to estimate
#' @param B integer; number of bootstrap replicates
#' @return A list with components \code{orig} with the original statistic and
#' \code{bs} a matrix whose columns are the bootstrapped statistics
#' @importFrom matrixStats colQuantiles
#' @export
bs_quantile <- function(vec,probs,B = 1000){
  n <- length(vec)
  vec <- vec[!is.na(vec)]
  if (n == 1) return(rep(NA,B))
  orig <- quantile(x = vec,probs = probs)
  mat <- matrix(vec[sample(n,n*B,replace = TRUE)],nrow = n,ncol = B,byrow = FALSE)
  #mat <- replicate(n = B,sample(x = vec,size = length(vec),replace = TRUE))
  bs_vals <- matrixStats::colQuantiles(mat,probs = probs)
  list(orig = orig,bs = bs_vals)
}

#' Truncated log-normal distribution
#'
#' @param n Number of samples
#' @param m Median of log-normal distribution (see ?rlnorm)
#' @param s Standard deviation of log-normal distribution (see ?rlnorm)
#' @param trunc.val Value for truncating the distribution (define the maximum possible value)
#'
#' @return Vector of random samples from the distribution
#'
#' @export

rlnorm_trunc <- function(n, m, s, trunc.val=Inf){

  x <- rlnorm(n, m, s)

  over <- which(x > trunc.val)

  while(length(over)>0){
    x[over] <- rlnorm(length(over), m, s)
    over <- which(x > trunc.val)
  }

  return(x)
}

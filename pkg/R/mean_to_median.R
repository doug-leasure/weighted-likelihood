#' Convert log-normal mean to median (equations derived from ?rlnorm)
#'
#' @param mu Mean of log-normal distribution. If inverse = T, this argument is the median.
#' @param log_sigma Standard deviation of log-normal distribution
#' @param inverse Logical indicating if inverse should be calculated (i.e. convert a median to a mean)
#'
#' @return If inverse = F, it returns the median of the log-normal distribution. If inverse = T, it returns the mean of the log-normal.
#'
#' @export

mean_to_median <- function(mu, log_sigma, inverse=F){
  e.term <- exp( log_sigma^2 / 2 )
  if(inverse) x <- mu * e.term
  if(!inverse) x <- mu / e.term
  return(x)
}

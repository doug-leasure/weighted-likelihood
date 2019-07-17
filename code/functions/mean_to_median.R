# convert log-normal mean to median (equations derived from ?rlnorm)
mean_to_median <- function(mu, log_sigma, inverse=F){
  e.term <- exp( log_sigma^2 / 2 )
  if(inverse) x <- mu * e.term
  if(!inverse) x <- mu / e.term
  return(x)
}

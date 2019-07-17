# convert sigma to log_sigma for log-normal distribution (equations derived from ?rlnorm)
sig_to_log <- function(med, sigma){
  a <- med^2
  b <- -med^2
  c <- -sigma^2
  X1 <- (-b + sqrt((b^2) - (4 * a * c))) / (2 * a)
  if(!X1 > 0){
    X1 <- (-b - sqrt((b^2) - (4 * a * c))) / (2 * a)
  }
  log_sigma <- sqrt(log(X1))
  return(log_sigma)
}

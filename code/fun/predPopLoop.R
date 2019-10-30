predPopLoop <- function(sim, d, t){
  
  n <- length(sim$x)

  y <- matrix(NA, nrow=nrow(d), ncol=n)
  
  alpha <- d[,paste0('alpha[',t,']')]
  sigma <- d[,paste0('LOG_SIGMA[',t,']')]
  
  if('beta' %in% names(d)) {
    beta <- d[,'beta']
  } else {
    beta <- 0
  }
  
  for(i in 1:n){
    y[,i] <- predPop(x = sim$x[i],
                     area = sim$area[i],
                     alpha = alpha,
                     beta = beta,
                     sigma = sigma
                     )
  }
  return(y)
}
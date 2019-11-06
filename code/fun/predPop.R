predPop <- function(sim, d, t){
  
  # sample size
  n <- length(sim$x)
  
  # mcmc iterations
  niter <- nrow(d)
  
  # parameters
  alpha <- d[,paste0('alpha[',t,']')]
  sigma <- d[,paste0('LOG_SIGMA[',t,']')]
  
  if('beta' %in% names(d)) { beta <- d[,'beta']
  } else { beta <- 0 }
  
  # predictions
  N <- matrix(NA, nrow=niter, ncol=n)
  
  for(i in 1:n){
    Dbar <- alpha + beta * sim$x[i]
    
    D <- rlnorm_trunc(niter, Dbar, sigma)
    
    N[,i] <- rpois(niter, sim$A[i] * D)
  }
  
  # result
  return(N)
}
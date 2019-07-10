# jags initials
inits <- function(chains=1, dat, med=200, sigma=0.4){
  inits.out <- list()
  
  for (c in 1:chains){
    inits.i <- list()
    
    inits.i$med <- runif(dat$ntype, 50, 500)
    LOG_SIGMA <- runif(1, 0.1, 1.5)
    
    inits.i$log_sigma[1:dat$ntype] <- rep(sqrt(1/mean((1/LOG_SIGMA^2) / dat$w)), dat$ntype)
    
    inits.i$.RNG.name = "lecuyer::RngStream" #"base::Wichmann-Hill"
    inits.i$.RNG.seed = c
    inits.out[[c]] <- inits.i
  }
  return(inits.out)
}

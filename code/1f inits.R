# jags initials
inits <- function(chains=1, dat){
  inits.out <- list()
  
  for (c in 1:chains){
    inits.i <- list()
    
    inits.i$med <- runif(dat$ntype, dat$med_real/2, dat$med_real*2)
    LOG_SIGMA <- runif(1, dat$log_sigma_real/2, dat$log_sigma_real*2)
    
    inits.i$log_sigma[1:dat$ntype] <- rep(sqrt(1/mean((1/LOG_SIGMA^2) / dat$w)), dat$ntype)
    
    inits.i$.RNG.name = "lecuyer::RngStream" #"base::Wichmann-Hill"
    inits.i$.RNG.seed = c
    inits.out[[c]] <- inits.i
  }
  return(inits.out)
}

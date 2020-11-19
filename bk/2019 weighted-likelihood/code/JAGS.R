model{
  
  for(i in 1:n){
    
    # LIKELIHOOD FUNCTION
    
    # population size
    N[i] ~ dpois(D_[i] * A[i])
    
    # population density
    D_[i] ~ dlnorm(Dbar[i], log_tau[i])
    
    # regression for population density
    Dbar[i] <- alpha[type[i]] + beta * x[i] * toggleCov
    
    # posterior prediction
    Dhat[i] ~ dlnorm(Dbar[i], log_tau[i])
    Nhat[i] ~ dpois(Dhat[i] * A[i])
    
    # tau[i] = sample-specific estimate of precision (after weighting)
    log_tau[i] <- pow(log_sigma[type[i]],-2) * w[i]
    log_sig[i] <- sqrt( 1 / log_tau[i] )
  }
  
  ## PRIORS ##
  
  beta ~ dnorm(0, pow(10,-2))
  
  for(t in 1:2){
    # alpha = regression intercept
    alpha[t] ~ dnorm(0, pow(10,-2))
    
    # sigma = log standard deviation (before weighting)
    log_sigma[t] ~ dunif(1e-3, 1)
  }
  
  ## DERIVED QUANTITIES ##
  
  # NAT_SIGMA = LOG_SIGMA converted to natural scale
  for(t in 1:2){
    SIGMA[t] <- sqrt( exp( 2 * log(alpha[t]) + LOG_SIGMA[t]^2 ) * ( exp( LOG_SIGMA[t]^2 ) - 1 ) )
  }
  
  # LOG_SIGMA = sqrt weighted average of sig among samples
  LOG_SIGMA[1] <- sum( log_sig[itype1] * sqrt(w[itype1]) ) / sum( sqrt( w[itype1] ))
  LOG_SIGMA[2] <- sum( log_sig[itype2] * sqrt(w[itype2]) ) / sum( sqrt( w[itype2] )) 
}
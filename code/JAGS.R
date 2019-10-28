model{
  
  for(i in 1:n){
    
    # LIKELIHOOD FUNCTION
    y[i] ~ dlnorm(log(med[type[i]]), log_tau[i])
    
    # tau[i] = sample-specific estimate of precision (after weighting)
    log_tau[i] <- pow(log_sigma[type[i]],-2) * w[i]
    
    log_sig[i] <- sqrt( 1 / log_tau[i] )
  }
  
  ## PRIORS ##
  
  for(t in 1:ntype){
    # med = median (on natural scale) of lognormal distribution
    med[t] ~ dunif(0, 1e3)
    
    # sigma = log standard deviation (before weighting)
    log_sigma[t] ~ dunif(1e-3, 1)
  }
  
  ## DERIVED QUANTITIES ##
  
  for(t in 1:ntype){
    
    # prediction
    yhat[t] ~ dlnorm(log(med[t]), pow(LOG_SIGMA[t],-2))
    
    # NAT_SIGMA = LOG_SIGMA converted to natural scale
    SIGMA[t] <- sqrt( exp( 2 * log(med[t]) + LOG_SIGMA[t]^2 ) * ( exp( LOG_SIGMA[t]^2 ) - 1 ) )
  }
  
  # LOG_SIGMA = sqrt weighted average of sig among samples
  LOG_SIGMA[1] <- sum( log_sig[itype1] * sqrt(w[itype1]) ) / sum( sqrt( w[itype1] ))
  LOG_SIGMA[2] <- sum( log_sig[itype2] * sqrt(w[itype2]) ) / sum( sqrt( w[itype2] )) 
}
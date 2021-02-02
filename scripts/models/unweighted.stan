// This script contains the Stan code for the Bayesian model

data{
  int<lower=0> n;                // sample size
  vector<lower=0>[n] N;          // observed counts
}

parameters{
  real med;              // median (natural)
  real<lower=0> sigma;   // standard deviation (log)
}

model{

  // likelihood
  N ~ lognormal(log(med), sigma);
  
  // priors
  med ~ uniform(0, 1e3);
  sigma ~ uniform(0, 5);
}



// This script contains the Stan code for the Bayesian model

data{
  int<lower=0> n;                // sample size
  vector<lower=0>[n] N;          // observed counts
  vector<lower=0,upper=1>[n] w;  // sampling probabilities
}

transformed data{
  
  // model weights (scaled inverse sampling weights)
  vector<lower=0,upper=1>[n] m = inv(w) ./ sum(inv(w)); 
}

parameters{
  real<lower=0> med;     // median
  real<lower=0> theta;   // naive variance component
}

transformed parameters{
  
  // location-specific weighted precision
  vector<lower=0>[n] tau = m * pow(theta,-2);
}

model{

  // likelihood
  N ~ lognormal( log(med), sqrt(inv(tau)) );

  // priors
  med ~ uniform(0, 2e3);
  theta ~ uniform(0, 1);
}

generated quantities {
  
  // weighted average sigma
  real<lower=0> sigma = sum( sqrt(inv(tau)) .* sqrt(m) ) / sum( sqrt(m));
}



// This script contains the Stan code for the Bayesian model

data{
  int<lower=0> n;                // sample size
  vector<lower=0>[n] N;          // observed counts
  vector<lower=0,upper=1>[n] w;  // sampling probabilities
}

transformed data{
  
  // scaled inverse weights
  vector<lower=0,upper=1>[n] w_inv = inv(w) ./ sum(inv(w)); 
}

parameters{
  real<lower=0> med;     // median
  real<lower=0> theta;   // naive variance component
}

transformed parameters{
  
  // location-specific weighted sigma
  vector<lower=0>[n] w_sigma = sqrt( inv( w_inv * pow(theta,-2) ) );
}

model{

  // likelihood
  N ~ lognormal( log(med), w_sigma );

  // priors
  med ~ uniform(0, 2e3);
  theta ~ uniform(0, 1);
}

generated quantities {
  
  // weighted average sigma
  real<lower=0> sigma = sum( w_sigma .* sqrt(w_inv) ) / sum( sqrt(w_inv));
}



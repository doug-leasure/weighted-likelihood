// This script contains the Stan code for the Bayesian model

data{
  int<lower=0> n;                // sample size
  vector<lower=0>[n] N;          // observed counts
  vector<lower=0,upper=1>[n] w;  // sampling weights (probability of selection)
}

parameters{
  real med;              // median (natural)
  real<lower=0> sigma;   // standard deviation (log)
}

model{

  // weighted likelihood
  for(i in 1:n){
    target += lognormal_lpdf( N[i] | log(med), sigma ) / w[i];  
  }
  
  // priors
  med ~ uniform(0, 1e3);
  sigma ~ uniform(0, 5);
}



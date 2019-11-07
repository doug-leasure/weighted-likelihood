#' Posterior predictions from Bayesian model using fitted parameter values
#'
#' @param sim Population simiulation object (see ?simPop)
#' @param d JAGS model saved as a data frame (see ?mcmc_to_df)
#' @param t Settlement type where predictions are being made (t = 1 or 2)
#'
#' @return Returns a matrix containig a vector of posterior predictions for each location (columns represent locations, rows represent posterior samples)
#'
#' @export

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

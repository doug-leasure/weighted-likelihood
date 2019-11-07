#' Create initials for MCMC chains in JAGS
#'
#' @param dat JAGS data object
#' @param chains Number of MCMC chains that will be run
#'
#' @return A list of lists. Each sub-list includes an item for each parameter with initials
#'
#' @export

inits <- function(dat, chains=3){
  inits.out <- list()

  for (c in 1:chains){
    inits.i <- list()

    inits.i$alpha <- log(runif(2, 50, 500))
    inits.i$beta <- runif(1, -0.1, 0.1)
    inits.i$log_sigma <- runif(2, 0.01, 0.1)

    # LOG_SIGMA <- runif(1, 0.1, 1.5)
    # inits.i$log_sigma[1:dat$ntype] <- rep(sqrt(1/mean((1/LOG_SIGMA^2) / dat$w)), dat$ntype)

    inits.i$.RNG.name = "lecuyer::RngStream" #"base::Wichmann-Hill"
    inits.i$.RNG.seed = dat$seed+c
    inits.out[[c]] <- inits.i
  }
  return(inits.out)
}

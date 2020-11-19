#' Simulate a population
#'
#' Creates a list object with population sizes, population densities, settled areas, and covariate values for every spatial unit
#'
#' @param n Number of spatial units
#' @param med Median of population densities
#' @param sigma Standard deviation of population densities
#' @param beta Effect of covariate on population density
#' @param maxarea Maximum settled area possible for a spatial unit
#' @param maxpop Absolute maximum population density used to truncate a log-normal
#'
#' @return A list object with an item for population count (N), density (D), settled area (A), covariate (x), and simulation parameters (med, sigma, beta)
#'
#' @export

simPop <- function(n=1e3, med=100, sigma=50, beta=0.2, maxarea=10, maxpop=Inf){

  # parameters
  alpha <- log(med)
  sigma <- sig_to_log(med, sigma)

  # covariate
  x <- rnorm(n, 0, 1)

  # population density
  Dbar <- alpha + beta * x

  D <- rlnorm_trunc(n, Dbar, sigma, maxpop)

  # settled area
  A <- rbeta(n, 5, 5) * (maxarea-1) + 1 # runif(n, 1, maxarea)

  # make area correlate with density
  if(T & !maxarea==1){
    sortA <- sort(A)
    orderD <- order(-D)
    A <- rep(NA,n)
    for(i in 1:n) A[orderD[i]] <- sortA[i]

    # add noise
    Aorig <- A
    noise <- 0.03
    A <- A * exp(rnorm(n, 0, maxarea*noise))
    i <- which(A > maxarea)
    while(length(i) > 0){
      A[i] <- Aorig[i] * exp(rnorm(length(i), 0, maxarea*noise))
      i <- which(A > maxarea)
    }
  }

  # population size
  N <- rpois(n, D * A)

  # result
  return(list(N=N, D=D, A=A, x=x, alpha=alpha, beta=beta, sigma=sigma))
}

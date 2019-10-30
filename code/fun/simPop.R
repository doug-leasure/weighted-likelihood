simPop <- function(n=1e3, med=100, sigma=50, beta=0.2, maxarea=1, maxpop=Inf){
  
  # parameters
  alpha <- log(med)
  sigma <- sig_to_log(med, sigma)
  
  # covariates
  x <- rnorm(n, 0, 1)

  # model
  med <- alpha + beta * x
  D <- rlnorm_trunc(n, med, sigma, maxpop)
  
   # area
  area <- sort(runif(n, 1, maxarea))
  orderD <- order(D)
  A <- rep(NA,n)
  for(i in 1:n) A[orderD[i]] <- area[i]
  
  # population total
  N <- D * A
  
  # replace duplicates
  i <- which(duplicated(N) | duplicated(D))
  while(length(i) > 0){
    med <- alpha + beta * x[i]
    D[i] <- rlnorm_trunc(length(i), med, sigma, maxpop)
    N[i] <- D[i] * A[i]
    
    i <- which(duplicated(N) | duplicated(D))
  }
  
  # result
  return(list(N=N, D=D, x=x, area=area, alpha=alpha, beta=beta, sigma=sigma))
}

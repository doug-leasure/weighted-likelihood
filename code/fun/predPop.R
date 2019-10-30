predPop <- function(x, alpha, beta, sigma, area=1, maxpop=Inf){
  
  med <- alpha + beta * x
  
  y <- rlnorm_trunc(length(med), med, sigma, maxpop)
  
  y <- y * area

  return(y)
}
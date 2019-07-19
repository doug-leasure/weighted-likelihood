rlnorm_trunc <- function(n, m, s, t=Inf){
  
  x <- rlnorm(n, m, s)
  
  over <- which(x > t)
  
  while(length(over)>0){
    x[over] <- rlnorm(length(over), m, s)
    over <- which(x > t)
  }
  
  return(x)
}

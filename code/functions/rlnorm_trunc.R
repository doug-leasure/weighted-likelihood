rlnorm_trunc <- function(n, m, s, trunc.val=Inf){
  
  x <- rlnorm(n, m, s)
  
  over <- which(x > trunc.val)
  
  while(length(over)>0){
    x[over] <- rlnorm(length(over), m, s)
    over <- which(x > trunc.val)
  }
  
  return(x)
}

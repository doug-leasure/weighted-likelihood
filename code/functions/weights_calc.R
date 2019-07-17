# calculate weights
weights_calc <- function(numerator1, denominator, numerator2=NULL, inverse=F){
  
  weights <- numerator1 / denominator
  
  if(inverse){
    
    weights <- 1 / weights 
    
    if(!is.null(numerator2)) {
      n2 <- length(numerator2)
      weights <- c(weights, rep( sum(weights) / n2, n2 ) )
    }
    
  }
  weights <- weights / sum(weights)
  return(weights)
}

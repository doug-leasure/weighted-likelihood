totalpop <- function(x, jd, type, maxpop=Inf){
  med <- x[paste0('med[',type,']')]
  sig <- x[paste0('LOG_SIGMA[',type,']')]
  
  popdens <- rlnorm_trunc(jd$ntotal[type], log(med), sig)
  
  total <- sum(jd$a * popdens)
  return(total)
}


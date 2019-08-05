totalpop <- function(x, jd, type, maxpop=Inf){
  med <- x[paste0('med[',type,']')]
  sig <- x[paste0('LOG_SIGMA[',type,']')]
  
  popdens <- rlnorm_trunc(jd$ntotal[type], log(med), sig, maxpop)
  
  if(length(jd$a) == 1){
    a <- jd$a
  }else{
    a <- jd$a[1:jd$ntotal[type],type]
  }
    
  total <- sum(a * popdens)
  return(total)
}


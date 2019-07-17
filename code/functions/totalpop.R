totalpop <- function(x, jd, type){
  med <- x[paste0('med[',type,']')]
  sig <- x[paste0('LOG_SIGMA[',type,']')]
  total <- sum(jd$a * rlnorm(jd$ntotal[type], log(med), sig))
  return(total)
}


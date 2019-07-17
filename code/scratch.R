# cleanup
rm(list=ls())
gc()
cat("\014") 
try(dev.off())

set.seed(42)

# assess priors for log_sigma
med <- c(150,150)
log_sigma <- c(1e-6,3)
log_tau <- log_sig <- LOG_SIGMA <- SIGMA <- c()

jd <- readRDS(paste0('out/drc/jd_weighted.rds'))

n <- jd$n
w <- jd$w
ntype <- jd$ntype
type <- jd$type
itype1 <- jd$itype1
itype2 <- jd$itype2

pow <- function(x, e) x^(e)

for(i in 1:n){
  log_tau[i] <- pow(log_sigma[type[i]],-2) * w[i]
  log_sig[i] <- sqrt( 1 / log_tau[i] )
}

# LOG_SIGMA = sqrt weighted average of sig among samples
LOG_SIGMA[1] <- sum( log_sig[itype1] * sqrt(w[itype1]) ) / sum(sqrt( w[itype1] ))
LOG_SIGMA[2] <- ifelse(ntype==2, sum( log_sig[itype2] * sqrt(w[itype2]) ) / sum( sqrt(w[itype2]) ), 0)

LOG_SIGMA[1] == log_sigma[1] * length(itype1) / sum(sqrt( w[itype1] ))
LOG_SIGMA[2] == ifelse(ntype==2, log_sigma[2] * length(itype2) / sum( sqrt(w[itype2]) ), 0)

# SIGMA = LOG_SIGMA converted to natural scale
for(t in 1:ntype){
  SIGMA[t] <- sqrt( exp( 2 * log(med[t]) + LOG_SIGMA[t]^2 ) * ( exp( LOG_SIGMA[t]^2 ) - 1 ) )
}

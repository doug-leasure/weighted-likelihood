# cleanup
rm(list=ls())
gc()
cat("\014") 
try(dev.off())

set.seed(42)

# load packages
library(runjags);library(boot);library(rjags);library(mcmc)

# working directory
setwd('C:/RESEARCH/2018 GRID3 WorldPop/git/wpgp/weighted-likelihood')

# output directory
outdir <- 'out/drc/'

# initials function
source('code/1f inits.R')

mods <- c('random','weighted','weighted_naive','all')

for(m in mods){
  # data
  jd <- readRDS(paste0('out/drc/jd_',m,'.rds'))
  
  # monitor
  par.monitor <- c('med','SIGMA','LOG_SIGMA','yhat')
  
  # modules
  load.module('lecuyer')
  load.module('glm')
  
  # jags setup
  n.adapt <- 1e3
  n.burn <- 1e3
  n.iter <- 5e3
  thin <- 1
  n.chains <- 3
  
  init <- inits(n.chains, jd)
  
  # run jags
  jm <- run.jags(model='code/1b JAGS.R', 
                 monitor=par.monitor, 
                 data=jd, 
                 n.chains=n.chains, 
                 inits=init, 
                 thin=thin,
                 adapt=n.adapt,
                 burnin=n.burn,
                 sample=n.iter,
                 summarise=F, 
                 #keep.jags.files=TRUE,
                 method='parallel' #'rjags' #'rjparallel'
  )
  jm$init <- init
  jm$seed <- 42
  saveRDS(jm, paste0(outdir,'jm_',m,'.rds'))
  
  # check traceplots
  pdf(paste0(outdir,'trace_',m,'.pdf'))
  traceplot(jm$mcmc[,c(paste0('med[',1:2,']'),paste0('SIGMA[',1:2,']'))])
  dev.off()
  
  # mcmc.list to data.frame
  d <- as.data.frame(jm$mcmc[[1]])
  for(i in 2:length(jm$mcmc)){
    d <- rbind(d, jm$mcmc[[i]])
  }
  write.csv(d, file=paste0(outdir,'d_',m,'.csv'), row.names=F)
  
  #-----------------------------------------------#
  
  jpeg(paste0(outdir,'model_',m,'.jpg'))
  
  # yhat predictions
  yhat1 <- d$`yhat[1]`
  yhat2 <- d$`yhat[2]`
  
  # plot densities
  density_y1 <- density(jd$y[jd$type==1])
  density_y2 <- density(jd$y[jd$type==2])
  
  density_yhat1 <- density(yhat1)
  density_yhat2 <- density(yhat2)
  
  # xlim <- c(0, quantile(c(yhat1,yhat2), probs=0.99))
  xlim <- c(0, 1000)
  ylim <- c(0, max(density_y1$y, density_y2$y, density_yhat1$y, density_yhat2$y))
  
  plot(NA, xlim=xlim, ylim=ylim, main=paste('Model:',m), xlab='Population Density', ylab='Probability')
  
  lines(density_yhat1, col='black', lwd=2, lty=1)
  lines(density_y1, col='black', lwd=2, lty=3)
  
  lines(density_yhat2, col='darkgrey', lwd=2, lty=1)
  lines(density_y2, col='darkgrey', lwd=2, lty=3)
    

  legend('topright', legend=c('Urban posterior','Urban data', 'Rural posterior','Rural data'),
         col=c('black','black','darkgrey','darkgrey'),
         lwd=2,
         lty=c(1,3,1,3))
  
  dev.off()
  
}


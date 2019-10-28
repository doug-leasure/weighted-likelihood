jagsModel <- function(dir, areaAdjust=F){
  
  # data
  jd <- readRDS(paste0(dir,'jd.rds'))
  
  # set seed
  set.seed(jd$seed)
  
  # monitor
  par.monitor <- c('med','log_sigma','SIGMA','LOG_SIGMA','yhat')
  
  # modules
  load.module('lecuyer')
  load.module('glm')
  
  # jags setup
  n.adapt <- 1e3
  n.burn <- 1e3
  n.iter <- 5e3
  thin <- 1
  n.chains <- 3
  
  init <- inits(jd, n.chains)
  
  # run jags
  jm <- run.jags(model='code/JAGS.R', 
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
  jm$seed <- jd$seed
  saveRDS(jm, paste0(dir, 'jm.rds'))
  
  # check traceplots
  pdf(paste0(dir, '/trace.pdf'))
  traceplot(jm$mcmc[,c(paste0('med[',1:2,']'),paste0('log_sigma[',1:2,']'),paste0('SIGMA[',1:2,']'))])
  dev.off()
  
  # mcmc.list to data.frame
  d <- as.data.frame(jm$mcmc[[1]])
  for(i in 2:length(jm$mcmc)){
    d <- rbind(d, jm$mcmc[[i]])
  }
  
  # posterior for pop totals
  for(t in 1:jd$ntype){
    d[,paste0('poptotal[',t,']')] <- apply(X=d, 
                                           MARGIN=1, 
                                           FUN=totalpop, 
                                           jd=jd, 
                                           type=t,
                                           areaAdjust=areaAdjust)
    }
  
  # save to disk
  write.csv(d, file=paste0(dir, 'd.csv'), row.names=F)
}
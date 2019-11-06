jagsModel <- function(dir, toggleCov=T){
  
  # data
  jd <- readRDS(paste0(dir,'jd.rds'))
  
  jd$toggleCov <- as.numeric(toggleCov)
  
  # set seed
  set.seed(jd$seed)
  
  # monitor
  par.monitor <- c('alpha','beta','LOG_SIGMA','Dhat')
  
  if(jd$toggleCov==0) par.monitor <- par.monitor[-which(par.monitor=='beta')]
  
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
  tracenames <- varnames(jm$mcmc)[!grepl('hat',varnames(jm$mcmc))]
  traceplot(jm$mcmc[,tracenames])
  dev.off()
  
  # mcmc.list to data.frame
  d <- as.data.frame(jm$mcmc[[1]])
  for(i in 2:length(jm$mcmc)){
    d <- rbind(d, jm$mcmc[[i]])
  }
  
  # derive posterior for pop totals
  for(t in 1:2){
    print(paste0('Calculate type ',t,' population total'))
    
    sim <- readRDS(paste0(dir,'sim',t,'.rds'))
    
    Nhat <- predPop(sim=sim, d=d, t=t)
    
    d[,paste0('poptotal[',t,']')] <- apply(Nhat, 1, sum)
  }
  
  # save to disk
  saveRDS(d, file=paste0(dir, 'd.rds'))
}
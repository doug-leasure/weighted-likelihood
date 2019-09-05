# output directory
outdir <- 'out/zmb_with_areas/'

# initials function
source('code/functions/inits.R')
source('code/functions/totalpop_for_defined_areas.R')

mods <- c('random','weighted','weighted_naive','combined')

for(m in mods){
  print(paste0('#====== ',m,' ======#'))
  
  # data
  jd <- readRDS(paste0('out/zmb_with_areas/',m,'/jd.rds'))
  
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
  
  init <- inits(n.chains, jd)
  
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
  jm$seed <- 42
  saveRDS(jm, paste0(outdir, m, '/jm.rds'))
  
  # check traceplots
  pdf(paste0(outdir, m, '/trace.pdf'))
  traceplot(jm$mcmc[,c(paste0('med[',1:2,']'),paste0('log_sigma[',1:2,']'),paste0('SIGMA[',1:2,']'))])
  dev.off()
  
  # mcmc.list to data.frame
  d <- as.data.frame(jm$mcmc[[1]])
  for(i in 2:length(jm$mcmc)){
    d <- rbind(d, jm$mcmc[[i]])
  }
  
  # posterior for pop totals
  for(t in 1:jd$ntype){
    d[,paste0('poptotal[',t,']')] <- apply(d, 1, totalpop_for_defined_areas, jd, type = t, maxpop = 800)
  }
  
  # save to disk
  write.csv(d, file=paste0(outdir, m, '/d.csv'), row.names=F)
}


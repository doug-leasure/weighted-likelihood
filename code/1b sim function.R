# sim
sim <- function(sampling='weighted', model_weights=T,
                n.weighted=500, n.random=500, n.real=1e4,
                ntype=1, type1_prop=0.5, strat_samp=T,
                med1=300, sigma1=300, med2=100, sigma2=100, 
                outdir='sim'){
  # sampling='weighted'; model_weights=T;
  # n.weighted=500; n.random=500; n.real=1e4;
  # ntype=2; type1_prop=0.8; strat_samp=T;
  # med1=250; sigma1=500;
  # med2=1000; sigma2=500;
  # outdir='sim'
  
  # create directories
  outdir <- paste0('out/', outdir, '/')
  dir.create(outdir, showWarnings=F)
  
  # save arguments
  write.csv(data.frame(argument = c('sampling','n.weighted', 'n.random','ntype','type1_prop', 'med1', 'sigma1', 'med2', 'sigma2', 'outdir'),
                       value = c(sampling, n.weighted, n.random, ntype, type1_prop, med1, sigma1, med2, sigma2, outdir)),
            file=paste0(outdir,'args.csv'),
            row.names=F)
  
  #-------------------- simulate real population (i.e. census) ----------------------#
  
  # simulate real population distribution
  if(ntype==2) {
    type2_prop <- 1-type1_prop
    n.real1 <- round(n.real*type1_prop)
    n.real2 <- round(n.real*type2_prop)
    ntotal <- c(n.real1, n.real2)
  } else {
    n.real1 <- n.real
    ntotal <- c(n.real)
  }
  
  log_sigma1 <- sig_to_log(med1, sigma1)
  
  real <- real1 <- rlnorm(n.real1, log(med1), log_sigma1)
  
  if(ntype==2) {
    log_sigma2 <- sig_to_log(med2, sigma2)
    
    real2 <- rlnorm(n.real2, log(med2), log_sigma2)
    real2 <- real2[!real2 %in% real1]
    
    real <- c(real1, real2)
    
    saveRDS(real1, file=paste0(outdir, 'real1.rds'))
    saveRDS(real2, file=paste0(outdir, 'real2.rds'))
  } 
  
  saveRDS(real, file=paste0(outdir, 'real.rds'))
  
  #------------ sample from population (i.e. microcensus) --------------#
  
  weights <- weights1 <- weights_calc(numerator1=real1, denominator=sum(real1))
  
  if(ntype==2) {
    weights2 <- weights_calc(numerator1=real2, denominator=sum(real2))
    
    weights <- c(weights1, weights2)
  }
  
  if(!strat_samp | ntype==1){
    # draw a pop-weighted sample
    weighted <- sample(real, n.weighted, replace=F, prob=weights)
    
    # draw a random sample
    random <- sample(real, n.random, replace=F)
    
    # combine random + weighted
    random_weighted <- c(weighted, random)
    
  } else {
    i1 <- 1:n.real1
    i2 <- (n.real1):length(real)
    
    # draw a stratified pop-weighted sample
    n1 <- round(n.weighted*type1_prop)
    n2 <- n.weighted - n1
    weighted <- c(sample(real[i1], n1, replace=F, prob=weights[i1]), 
                  sample(real[i2], n2, replace=F, prob=weights[i2]))
    
    # draw a stratified random sample
    n1 <- round(n.random*type1_prop)
    n2 <- n.random - n1
    random <- c(sample(real[i1], n1, replace=F), 
                  sample(real[i2], n2, replace=F))
    
    # combine random + weighted
    random_weighted <- c(weighted, random)
  }
  
  # inverse weights for sample
  if(sampling=='random'){
    inv.weights <- rep(1/n.random, n.random)
    y <- random
    saveRDS(random, paste0(outdir, 'random.rds'))
  }
  if(sampling=='weighted'){
    inv.weights <- weights_calc(numerator1=weighted, denominator=sum(real), inverse=T)
    y <- weighted
    saveRDS(weighted, paste0(outdir, 'weighted.rds'))
  } 
  if(sampling=='combined'){
    inv.weights <- weights_calc(numerator1=weighted, denominator=sum(real), numerator2=random, inverse=T)
    y <- c(weighted, random)
    
    saveRDS(random, paste0(outdir, 'random.rds'))
    saveRDS(weighted, paste0(outdir, 'weighted.rds'))
    saveRDS(random_weighted, paste0(outdir, 'random_weighted.rds'))
  }
  
  if(!model_weights) inv.weights <- rep(1/length(y), length(y))
  
  # type of each sample
  if(ntype==2) {
    type <- rep(NA, length(y))
    type[y %in% real1] <- 1
    type[y %in% real2] <- 2
    itype1 <- which(type==1)
    itype2 <- which(type==2)
  } else {
    type <- rep(1, length(y))
    itype1 <- which(type==1)
    itype2 <- 1
  }
  
  #--------------- Bayesian model -------------------#
  
  # jags data
  jd <- list(n = length(y),
             y = y,
             a = 1,
             ntotal = ntotal,
             w = inv.weights,
             type = type,
             ntype = ntype,
             itype1 = itype1,
             itype2 = itype2
  )
  saveRDS(jd, paste0(outdir,'jd.rds'))
  
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
  saveRDS(jm, paste0(outdir,'jm.rds'))
  
  # traceplots
  pdf(paste0(outdir,'trace.pdf'))
  if(ntype==2) {
    traceplot(jm$mcmc[,c(paste0('med[',1:2,']'),paste0('log_sigma[',1:2,']'),paste0('SIGMA[',1:2,']'))])
  } else {
    traceplot(jm$mcmc[,c('med','log_sigma','SIGMA')])
  }
  dev.off()
  
  # mcmc.list to data.frame
  d <- as.data.frame(jm$mcmc[[1]])
  for(i in 2:length(jm$mcmc)){
    d <- rbind(d, jm$mcmc[[i]])
  }
  
  # posterior pop totals
  for(t in 1:jd$ntype){
    d[,paste0('poptotal[',t,']')] <- apply(d, 1, totalpop, jd, t)
  }
  
  # save to disk
  write.csv(d, file=paste0(outdir,'d.csv'), row.names=F)
  
}
  

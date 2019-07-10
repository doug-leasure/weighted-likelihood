# sim
sim <- function(sampling='weighted', model_weights=T,
                n.weighted=500, n.random=500, n.real=1e4,
                ntype=1, type1_prop=0.5, strat_samp=T,
                med1=750, sigma1=500, med2=100, sigma2=250, 
                outdir='sim'){
  # sampling='weighted'; model_weights=T;
  # n.weighted=500; n.random=500; n.real=1e4;
  # ntype=2; type1_prop=0.8; strat_samp=T;
  # med1=250; sigma1=500;
  # med2=1000; sigma2=500;
  # outdir='sim'
  
  outdir <- paste0('out/', outdir, '/')
  dir.create(outdir, showWarnings=F)
  
  #------------------------------------------#
  
  write.csv(data.frame(argument = c('sampling','n.weighted', 'n.random','ntype','type1_prop', 'med1', 'sigma1', 'med2', 'sigma2', 'outdir'),
                       value = c(sampling, n.weighted, n.random, ntype, type1_prop, med1, sigma1, med2, sigma2, outdir)),
            file=paste0(outdir,'args.csv'),
            row.names=F)
  
  #------------------------------------------#
  
  # simulate real population distribution
  if(ntype==2) {
    type2_prop <- 1-type1_prop
    n.real1 <- round(n.real*type1_prop)
    n.real2 <- round(n.real*type2_prop)
  } else {
    n.real1 <- n.real
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
  
  #--------------------------#
  
  weights <- weights_calc(numerator1=real, denominator=sum(real))
  
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
  
  
  #-------------------------------#
  
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
  
  #----------------------------------#
  # JAGS
  
  # data
  jd <- list(n = length(y),
             y = y,
             w = inv.weights,
             type = type,
             ntype = ntype,
             itype1 = itype1,
             itype2 = itype2,
             med_real = median(real),
             log_sigma_real = sig_to_log(median(real), sd(real))
  )
  saveRDS(jd, paste0(outdir,'jd.rds'))
  
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
  set.seed(123)
  
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
  jm$seed <- 123
  saveRDS(jm, paste0(outdir,'jm.rds'))
  
  # check traceplots
  pdf(paste0(outdir,'trace.pdf'))
  if(ntype==2) {
    traceplot(jm$mcmc[,c(paste0('med[',1:2,']'),paste0('SIGMA[',1:2,']'))])
  } else {
    traceplot(jm$mcmc[,c('med','SIGMA')])
  }
  dev.off()
  
  # mcmc.list to data.frame
  d <- as.data.frame(jm$mcmc[[1]])
  for(i in 2:length(jm$mcmc)){
    d <- rbind(d, jm$mcmc[[i]])
  }
  write.csv(d, file=paste0(outdir,'d.csv'), row.names=F)
  
  #-----------------------------------------------#
  
  jpeg(paste0(outdir,'model.jpg'))
  if(ntype==2){
    # yhat predictions
    yhat1 <- d$`yhat[1]`
    yhat2 <- d$`yhat[2]`
    
    # plot densities
    density_y1 <- density(jd$y[jd$type==1])
    density_y2 <- density(jd$y[jd$type==2])
    
    density_real1 <- density(real1)
    density_real2 <- density(real2)
    
    density_yhat1 <- density(yhat1)
    density_yhat2 <- density(yhat2)
    
    xlim <- c(0, quantile(real, probs=0.99))
    ylim <- c(0, max(density_y1$y, density_y2$y, density_real1$y, density_real2$y, density_yhat1$y, density_yhat2$y))
    
    plot(NA, xlim=xlim, ylim=ylim, xlab='Population', ylab='Probability')
    
    lines(density_real1, col='gray', lwd=2, lty=2)
    lines(density_real2, col='gray', lwd=2, lty=2)
    
    lines(density_yhat1, col='black', lwd=2, lty=1)
    lines(density_yhat2, col='black', lwd=2, lty=1)

    lines(density_y1, col='black', lwd=2, lty=3)
    lines(density_y2, col='black', lwd=2, lty=3)
    
  } else {
    
    # yhat predictions
    yhat1 <- d$yhat
    
    # plot densities
    density_real1 <- density(real1)
    
    density_y <- density(jd$y)
    
    density_yhat1 <- density(yhat1)
    
    xlim <- c(0, quantile(real, probs=0.99))
    ylim <- c(0, max(density_y$y, density_real1$y, density_yhat1$y))
    
    plot(NA, xlim=xlim, ylim=ylim, xlab='Population',ylab='Probability')
    
    lines(density_real1, col='gray', lwd=2, lty=2)
    lines(density_yhat1, col='black', lwd=2, lty=1)
    lines(density_y, col='black', lwd=2, lty=3)
  }
  
  legend('topright', legend=c('True','Estimated','Sample'),
         col=c('gray','black','black'),
         lwd=c(2,2,2),
         lty=c(2,1,3))
  
  dev.off()
  
  #-----------------------------------#
  
  jpeg(paste0(outdir,'totals.jpg'))
  
  if(ntype==1){
    total1 <- sum(rlnorm(n.real1, log(mean(d$`med`)), mean(d$`LOG_SIGMA[1]`) ) )
    
    barplot(height=matrix(c(sum(real1), total1), ncol=1, byrow=T), 
            beside=T, space=0.1,
            legend.text=c('True','Estimated'),
            main='Population Total')
  }
  
  if(ntype==2){
    total1 <- sum(rlnorm(n.real1, log(mean(d$`med[1]`)), mean(d$`LOG_SIGMA[1]`) ) )
    total2 <- sum(rlnorm(n.real2, log(mean(d$`med[2]`)), mean(d$`LOG_SIGMA[2]`) ) )
    
    barplot(height=matrix(c(sum(real1), sum(real2), total1, total2), ncol=2, byrow=T), 
            beside=T,
            names=c('Type 1', 'Type 2'),
            legend.text=c('True','Estimated'),
            main='Population Total')
  } 
   
  dev.off()
  
}
  

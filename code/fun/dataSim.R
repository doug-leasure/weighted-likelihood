dataSim <- function(sampling='weighted', model_weights=T,
                n.weighted=500, n.random=500, n.real=1e4,
                type1_prop=0.5, strat_samp=T,
                med1=300, sigma1=300, med2=100, sigma2=100, 
                outdir='sim', seed=42){
  # sampling='weighted'; model_weights=T;
  # n.weighted=500; n.random=500; n.real=1e4;
  # type1_prop=0.8; strat_samp=T;
  # med1=250; sigma1=500;
  # med2=1000; sigma2=500;
  # outdir='sim'
  
  # create directory
  dir.create(outdir, showWarnings=F)
  
  # save arguments
  write.csv(data.frame(argument = c('sampling','n.weighted', 'n.random','type1_prop', 'med1', 'sigma1', 'med2', 'sigma2', 'outdir','seed'),
                       value = c(sampling, n.weighted, n.random, type1_prop, med1, sigma1, med2, sigma2, outdir, seed)),
            file=paste0(outdir,'args.csv'),
            row.names=F)
  
  #-------------------- simulate real population (i.e. census) ----------------------#
  
  # simulate real population distribution
  type2_prop <- 1-type1_prop
  
  n.real1 <- round(n.real*type1_prop)
  n.real2 <- round(n.real*type2_prop)
  ntotal <- c(n.real1, n.real2)

  log_sigma1 <- sig_to_log(med1, sigma1)
  log_sigma2 <- sig_to_log(med2, sigma2)
  
  real1 <- rlnorm_trunc(n.real1, log(med1), log_sigma1)
  real2 <- rlnorm_trunc(n.real2, log(med2), log_sigma2)
  real2 <- real2[!real2 %in% real1]
  
  saveRDS(real1, file=paste0(outdir, 'real1.rds'))
  saveRDS(real2, file=paste0(outdir, 'real2.rds'))

  real <- c(real1, real2)
  saveRDS(real, file=paste0(outdir, 'real.rds'))
  
  #------------ sample from population (i.e. microcensus) --------------#
  
  weights1 <- weights_calc(numerator1=real1, denominator=sum(real1))
  
  weights2 <- weights_calc(numerator1=real2, denominator=sum(real2))
  
  if(!strat_samp){
    # draw a pop-weighted sample
    weighted <- sample(real, n.weighted, replace=F, prob=weights)
    
    # draw a random sample
    random <- sample(real, n.random, replace=F)
    
    # combine random + weighted
    random_weighted <- c(weighted, random)
    
  } else {
    
    # draw a stratified pop-weighted sample
    n1 <- round(n.weighted*type1_prop)
    n2 <- n.weighted - n1
    weighted <- c(sample(real1, n1, replace=F, prob=weights1), 
                  sample(real2, n2, replace=F, prob=weights2))
    
    # draw a stratified random sample
    n1 <- round(n.random*type1_prop)
    n2 <- n.random - n1
    random <- c(sample(real1, n1, replace=F), 
                  sample(real2, n2, replace=F))
    
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
  type <- rep(NA, length(y))
  type[y %in% real1] <- 1
  type[y %in% real2] <- 2
  itype1 <- which(type==1)
  itype2 <- which(type==2)

  # jags data
  jd <- list(n = length(y),
             y = y,
             w = inv.weights,
             ntype = 2,
             type = type,
             itype1 = itype1,
             itype2 = itype2,
             ntotal = ntotal,
             a = 1,
             seed = seed
  )
  saveRDS(jd, paste0(outdir,'jd.rds'))
}
  

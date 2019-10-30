dataSim <- function(sampling='weighted', model_weights=T,
                n.weighted=500, n.random=500, n.real=1e4, 
                maxarea=10, beta=0.2,
                type1_prop=0.5, med1=300, sigma1=150, 
                med2=100, sigma2=50, 
                outdir='sim', seed=42){
  
  # create directory
  dir.create(outdir, showWarnings=F)
  
  # save arguments
  write.csv(data.frame(argument = c('sampling','n.weighted', 'n.random','type1_prop', 'med1', 'sigma1', 'med2', 'sigma2', 'outdir','seed'),
                       value = c(sampling, n.weighted, n.random, type1_prop, med1, sigma1, med2, sigma2, outdir, seed)),
            file=paste0(outdir,'args.csv'),
            row.names=F)
  
  ##---- simulate population (i.e. census) ----##
  
  type2_prop <- 1-type1_prop
  
  n.real1 <- round(n.real*type1_prop)
  n.real2 <- round(n.real*type2_prop)
  ntotal <- c(n.real1, n.real2)

  sim1 <- simPop(n=n.real1, med=med1, sigma=sigma1, beta=beta, maxarea=maxarea)
  sim2 <- simPop(n=n.real2, med=med2, sigma=sigma2, beta=beta, maxarea=maxarea)
  
  saveRDS(sim1, file=paste0(outdir, 'sim1.rds'))
  saveRDS(sim2, file=paste0(outdir, 'sim2.rds'))
  
  real1 <- sim1$D
  real2 <- sim2$D
  
  real <- c(real1, real2)
  
  saveRDS(real1, file=paste0(outdir, 'real1.rds'))
  saveRDS(real2, file=paste0(outdir, 'real2.rds'))
  saveRDS(real, file=paste0(outdir, 'real.rds'))
  
  ##---- sample from population ----##
  if(sampling %in% c('random','combined')){
    
    # draw a stratified random sample
    n1 <- round(n.random*type1_prop)
    n2 <- n.random - n1
    
    idx.rsamp1 <- sample(length(real1), n1, replace=F)
    idx.rsamp2 <- sample(length(real2), n2, replace=F)
    
    rsamp1 <- real1[idx.rsamp1]
    rsamp2 <- real2[idx.rsamp2]
    
    random <- c(rsamp1, rsamp2)
    
    type.random <- c( rep(1,n1) , rep(2,n2) )
    x.random <- c( sim1$x[idx.rsamp1] , sim2$x[idx.rsamp2] )
    area.random <- c( sim1$area[idx.rsamp1] , sim2$area[idx.rsamp2] )
    
    saveRDS(random, paste0(outdir, 'random.rds'))
    
    if(sampling=='random'){
      inv.weights <- rep(1/n.random, n.random)
      y <- random
      x <- x.random
      area <- area.random
      type <- type.random
    }
  }
  if(sampling %in% c('weighted','combined')){
    
    # weights
    weights1 <- weights_calc(numerator1=real1, denominator=sum(real1))
    weights2 <- weights_calc(numerator1=real2, denominator=sum(real2))
    
    # draw a stratified pop-weighted sample
    n1 <- round(n.weighted*type1_prop)
    n2 <- n.weighted - n1
    
    idx.wsamp1 <- sample(length(real1), n1, replace=F, prob=weights1)
    idx.wsamp2 <- sample(length(real2), n2, replace=F, prob=weights2)
    
    wsamp1 <- real1[idx.wsamp1]
    wsamp2 <- real2[idx.wsamp2]
    
    weighted <- c(wsamp1, wsamp2)
    
    type.weighted <- c( rep(1,n1) , rep(2,n2) )
    x.weighted <- c( sim1$x[idx.wsamp1] , sim2$x[idx.wsamp2] )
    area.weighted <- c( sim1$area[idx.wsamp1] , sim2$area[idx.wsamp2] )
    
    saveRDS(weighted, paste0(outdir, 'weighted.rds'))
    
    if(sampling=='weighted'){
      inv.weights <- weights_calc(numerator1=weighted, denominator=sum(real), inverse=T)
      y <- weighted
      x <- x.weighted
      area <- area.weighted
      type <- type.weighted
    }
  } 
  if(sampling=='combined'){
    
    # combine random + weighted
    y <- c(weighted, random)
    type <- c(type.weighted, type.random)
    x <- c(x.weighted, x.random)
    area <- c(area.weighted, area.random)
    
    inv.weights <- weights_calc(numerator1=weighted, denominator=sum(real), numerator2=random, inverse=T)
    
    saveRDS(y, paste0(outdir, 'random_weighted.rds'))
  }
  
  if(!model_weights) inv.weights <- rep(1/length(y), length(y))
  
  # jags data
  jd <- list(n = length(y),
             y = y,
             w = inv.weights,
             x = x,
             a = area,
             type = type,
             itype1 = which(type==1),
             itype2 = which(type==2),
             ntotal = ntotal,
             seed = seed
  )
  saveRDS(jd, paste0(outdir,'jd.rds'))
}
  

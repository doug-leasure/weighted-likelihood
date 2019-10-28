plotTotalsData <- function(dir, alpha=0.05, plotReal=F){
  
  # sims
  names <- c('random','weighted_naive','weighted','combined')
  sims <- paste0(outdir, names)

  # load data
  d <- real1 <- real2 <- list()
  for(i in 1:length(names)){
    d[[names[i]]] <- read.csv(paste0(sims[i],'/d.csv'), check.names=F)
    
    if(plotReal){
      real1[[names[i]]] <- readRDS(paste0(sims[i],'/real1.rds'))
      real2[[names[i]]] <- readRDS(paste0(sims[i],'/real2.rds')) 
    }
  }
  
  # output data
  real <- totals <- upper <- lower <- data.frame(urban=rep(NA, 4),
                                                 rural=rep(NA, 4),
                                                 row.names=names)
  for(i in names){
    
    # real totals
    if(plotReal){
      real[i,'urban'] <- sum(real1[[i]])
      real[i,'rural'] <- sum(real2[[i]])
    }
    
    # estimated totals
    totals[i,'urban'] <- mean(d[[i]]$`poptotal[1]`)
    totals[i,'rural'] <- mean(d[[i]]$`poptotal[2]`)
    
    # upper
    upper[i,'urban'] <- quantile(d[[i]]$`poptotal[1]`, probs=c(0.975))
    upper[i,'rural'] <- quantile(d[[i]]$`poptotal[2]`, probs=c(0.975))
    
    # lower
    lower[i,'urban'] <- quantile(d[[i]]$`poptotal[1]`, probs=c(0.025))
    lower[i,'rural'] <- quantile(d[[i]]$`poptotal[2]`, probs=c(0.025))
  }
  
  return(list(real=real, totals=totals, upper=upper, lower=lower))
}

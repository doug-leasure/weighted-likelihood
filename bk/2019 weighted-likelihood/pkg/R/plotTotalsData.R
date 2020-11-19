#' Plot population totals estimated from each simulated sampling design
#' 
#' @param dir Directory for where simulation results are located (required)
#' @param alpha Parameter to define confidence limit (Default = 0.05 for 95% confidence intervals)
#' @param plotReal Toggle a line to show the true population size from simulations (Default = F)
#' 
#' @return Results are saved to disk as jpeg file.
#' 
#' @export

plotTotalsData <- function(dir, alpha=0.05, plotReal=F){
  
  # sims
  names <- c('random','weighted_naive','weighted','combined')
  sims <- file.path(dir, names)

  # load data
  d <- real1 <- real2 <- list()
  for(i in 1:length(names)){
    d[[names[i]]] <- readRDS(file.path(sims[i],'d.rds'))
    
    if(plotReal){
      real1[[names[i]]] <- readRDS(file.path(sims[i],'sim1.rds'))$N
      real2[[names[i]]] <- readRDS(file.path(sims[i],'sim2.rds'))$N
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

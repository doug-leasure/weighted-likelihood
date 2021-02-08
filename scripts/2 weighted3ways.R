# cleanup
rm(list=ls());gc();cat("\014");try(dev.off())

# packages
library(rstan);library(runjags)
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)

# # patch for stan glitch: https://discourse.mc-stan.org/t/new-error-cleanup-makevar-old-argument-rmu-is-missing-with-no-default/18137/21
# file.rename("~/.R/Makevars.win", "~/.R/Makevars.win.bak")

# working directory
setwd(file.path(dirname(rstudioapi::getSourceEditorContext()$path),'../wd'))

# simulation parameters
n_population <- 1e6
meds <- c(100, 250, 500) # c(100, 250, 500, 750, 1000)
sigmas <- c(0.25, 0.5, 0.75) # c(0.25, 0.5, 0.75, 1)
n <- 2e3

# function: initials
init <- function(c, d, med, sigma){
  result <- list()
  for(i in 1:c){
    result[[i]] <- list(med = runif(1, med/2, min(1e3,med*1.5)),
                        sigma = runif(1, sigma/2, min(1,sigma*1.5)))
    
    result[[i]]$theta <- sqrt( mean(d$w) / result[[i]]$sigma^-2 )
    
    result[[i]]$.RNG.name = "lecuyer::RngStream" #"base::Wichmann-Hill"
    result[[i]]$.RNG.seed = md$seed + c
  }
  return(result)
}

# function: remove parameters from initials
rm_init <- function(rm_names, init_list){
  for(i in 1:length(init_list)){
    init_list[[i]] <- init_list[[i]][-which(names(init_list[[i]]) %in% rm_names)]
  }
  return(init_list)
}

# function: plot results
myplot <- function(pop, dat, hatA, hatB, hatC){
  
  # density
  d <- list(pop = density(pop),
            dat = density(dat),
            hatA = density(hatA),
            hatB = density(hatB),
            hatC = density(hatC))
  
  # axis limits
  xlim <- c(0, max(dat))

  ylim <- range(d[[1]]$y)
  for(i in 2:length(d)){
    rng <- range(d[[i]]$y)
    if(rng[1] < ylim[1]) ylim[1] <- rng[1]
    if(rng[2] > ylim[2]) ylim[2] <- rng[2]
  }

  plot(NA, 
       main = NULL, 
       ylab = 'Probability Density', 
       xlab = 'People per Hectare', 
       xlim = xlim, 
       ylim = ylim)
  
  polygon(y=c(d[['pop']]$y, rep(0,length(d[['pop']]$y))),
          x=c(d[['pop']]$x, rev(d[['pop']]$x)) , col=gray(0.6, alpha=0.5), border=NA)
  
  
  lines(d[['dat']], lty=3)
  
  lines(d[['hatA']], lty=1, col='black')
  lines(d[['hatB']], lty=1, col='#0072B2')
  lines(d[['hatC']], lty=1, col='#D55E00')
  
  legend('topright', 
         legend = c('population','weighted sample','weighted likelihood (Stan)','weighted precision (Stan)', 'weighted precision (JAGS)'),
         fill=c(gray(0.6),NA,NA,NA,NA),
         border=c(NA,NA,NA,NA,NA),
         lty = c(NA,3,1,1,1),
         lwd = c(NA,1,1,1,1),
         col = c(gray(0.6, alpha=0.5),'black', 'black', "#0072B2", "#D55E00"))
}


##-------------------------------------------##

for(med in meds){
  for(sigma in sigmas){
    
    # output directories
    simname <- paste0('med',med,'sigma',sigma)
    outdir <- file.path('weighted3ways',simname)
    dir.create(outdir, recursive=T, showWarnings=F)
    
    # population count per 100 m pixel
    population <- rlnorm(n_population, log(med), sigma)
    
    # sampling weights
    weights <- population
    weights <- weights / sum(weights)
    
    # draw sample
    i_sample <- sample(x = 1:length(population),
                       size = n,
                       prob = weights)
    
    pop_sample <- population[i_sample]
    
    weights_sample <- weights[i_sample]
    
    # compare true population to weighted sample
    median(population)
    median(pop_sample)
    
    mean(population)
    mean(pop_sample)
    
    sd(population)
    sd(pop_sample)
    
    # model data
    md <- list(N = pop_sample,
               w = weights_sample,
               n = n,
               seed = runif(1,1,4242))
    
    # mcmc
    pars <- c('med','sigma')
    chains <- 4
    warmup <- 1000
    iter <- 1000

    # initials
    inits <- init(chains, md, med, sigma)
    
    ##--------------##
    
    # fit Stan weighted likelihood model
    fitA <- rstan::stan(file = '../models/weighted_likelihood.stan',
                        data = md,
                        chains = chains,
                        iter = warmup + iter,
                        warmup = warmup,
                        pars = pars,
                        init = rm_init(c('theta','.RNG.name','.RNG.seed'),inits),
                        seed = md$seed)
    
    # trace plots
    jpeg(file.path(outdir,paste0('traceA_',simname,'.jpg')))
      print(rstan::traceplot(fitA))
    dev.off()
      
    # fit to df
    dfA <- as.data.frame(fitA)
    
    # predictions
    hatA <- rlnorm(nrow(dfA), log(dfA$med), dfA$sigma)
    
    
    ##--------------##
    
    # fit Stan weighted precision model
    fitB <- rstan::stan(file = '../models/weighted_precision.stan',
                        data = md,
                        chains = chains,
                        iter = warmup + iter,
                        warmup = warmup,
                        pars = pars,
                        init = rm_init(c('sigma','.RNG.name','.RNG.seed'),inits),
                        seed = md$seed)
    
    # trace plots
    jpeg(file.path(outdir,paste0('traceB_',simname,'.jpg')))
      print(rstan::traceplot(fitB))
    dev.off()
    
    # fit to df
    dfB <- as.data.frame(fitB)
    
    # predictions
    hatB <- rlnorm(nrow(dfB), log(dfB$med), dfB$sigma)
    
    ##--------------##
    
    # fit JAGS weighted precision model
    fitC <- run.jags(model = '../models/weighted_precision.jags',
                     data = md,
                     n.chains = chains,
                     sample = iter,
                     burnin = warmup,
                     monitor = pars,
                     inits = rm_init('sigma',inits),
                     thin = 1,
                     summarise = F,
                     method = 'parallel'
    )
    
    # trace plots
    jpeg(file.path(outdir,paste0('traceC_',simname,'.jpg')))
      plot(fitC$mcmc)
    dev.off()
    
    # fit to df
    dfC <- as.data.frame(fitC$mcmc[[1]])
    for(i in 2:length(fitC$mcmc)){
      dfC <- rbind(dfC, fitC$mcmc[[i]])
    }
    
    # predictions
    hatC <- rlnorm(nrow(dfC), log(dfC$med), dfC$sigma)
    
    ##--------------##
    
    # plot results
    jpeg(file.path(outdir,paste0('results_',simname,'.jpg')), res=600, height=6, width=6, units='in')
      myplot(pop=population, dat=pop_sample, hatA=hatA, hatB=hatB, hatC=hatC)
    dev.off()
    
    ##--------------##
    
    ## compare results: true population, sample data, model estimate
    
    result <- data.frame(row.names = c('population','sample','weighted_likelihood','weighted_precision_stan','weighted_precision_jags'))
    result[,c('mean','median','sigma')] <- NA
    
    # median
    result['population','median'] <- median(population)
    result['sample','median'] <- median(pop_sample)
    result['weighted_likelihood','median'] <- median(hatA)
    result['weighted_precision_stan','median'] <- median(hatB)
    result['weighted_precision_jags','median'] <- median(hatC)
    
    # mean
    result['population','mean'] <- mean(population)
    result['sample','mean'] <- mean(pop_sample)
    result['weighted_likelihood','mean'] <- mean(hatA)
    result['weighted_precision_stan','mean'] <- mean(hatB)
    result['weighted_precision_jags','mean'] <- mean(hatC)
    
    # sigma
    result['population','sigma'] <- sd(population)
    result['sample','sigma'] <- sd(pop_sample)
    result['weighted_likelihood','sigma'] <- sd(hatA)
    result['weighted_precision_stan','sigma'] <- sd(hatB)
    result['weighted_precision_jags','sigma'] <- sd(hatC)
    
    # save
    write.csv(result, file=file.path(outdir,paste0('results_',simname,'.csv')))
    
  }
}


# cleanup
rm(list=ls())
gc()
cat("\014") 
try(dev.off())

# seed
seed <- runif(1, 0, 42)
set.seed(seed)

# load packages
library(runjags);library(boot);library(rjags);library(mcmc)

# working directory
setwd('C:/RESEARCH/git/wpgp/weighted-likelihood')
# setwd('//filestore.soton.ac.uk/users/cad1c14/mydocuments/GitHub/weighted-likelihood')

# source functions
for(i in list.files('code/fun') ) source(paste0('code/fun/',i))

# create directories
dir.create('out', showWarnings=F)

for(toggleCov in 0:1){
  
  outdir <- paste0('out/sims_cov',toggleCov,'/')
  dir.create(outdir, showWarnings=F)
  
  # simulation parameters
  maxarea <- 10 
  beta <- 0.2
  med1 <- 300 
  sigma1 <- 150 
  med2 <- 100
  sigma2 <- 100
  
  simParms <- list(random = list(sampling='random',
                                 n.random=1000,
                                 n.weighted=0,
                                 outdir=paste0(outdir,'random/'),
                                 maxarea=maxarea,beta=beta,med1=med1,sigma1=sigma1,med2=med2,sigma2=sigma2,seed=seed),
                   
                   weighted_naive = list(sampling='weighted',
                                         n.random=0,
                                         n.weighted=1000,
                                         model_weights=F, 
                                         outdir=paste0(outdir,'weighted_naive/'),
                                         maxarea=maxarea,beta=beta,med1=med1,sigma1=sigma1,med2=med2,sigma2=sigma2,seed=seed),
                   
                   weighted = list(sampling='weighted',
                                   n.random=0,
                                   n.weighted=1000,
                                   outdir=paste0(outdir,'weighted/'),
                                   maxarea=maxarea,beta=beta,med1=med1,sigma1=sigma1,med2=med2,sigma2=sigma2,seed=seed),
                   
                   combined = list(sampling='combined', 
                                   n.random=500, 
                                   n.weighted=500, 
                                   outdir=paste0(outdir,'combined/'),
                                   maxarea=maxarea,beta=beta,med1=med1,sigma1=sigma1,med2=med2,sigma2=sigma2,seed=seed)
                   )

  for(i in names(simParms)){
    # simulate population
    do.call(dataSim, simParms[[i]])
    
    # fit models
    jagsModel(paste0(outdir,i,'/'), toggleCov=toggleCov)
  }
  
  # plot (4 panel) of data and model
  plotModelPanel(file=paste0(outdir,'sim_model.jpg'), 
                 sims=names(simParms), 
                 dir=outdir, 
                 plotReal=T)
  
  # plot of population totals for each model
  plotTotals(dat = plotTotalsData(dir=outdir, plotReal=T),
             file = paste0(outdir,'sim_totals.jpg'),
             plotReal=T
  )
}


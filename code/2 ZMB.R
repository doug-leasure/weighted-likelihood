# cleanup
rm(list=ls())
gc()
cat("\014") 
try(dev.off())

# seed
seed <- runif(1, 0, 42)
set.seed(seed)

# packages
library(runjags); library(rjags); library(dplyr); library(sf); library(reshape2); library(tidyr); library(purrr); library(svDialogs)

# working directory
setwd('C:/RESEARCH/git/wpgp/weighted-likelihood')
# setwd('//filestore.soton.ac.uk/users/cad1c14/mydocuments/GitHub/weighted-likelihood')

# simulation names
sims <- c('random','weighted_naive','weighted','combined')

# functions
for(i in list.files('code/fun')) source(paste0('code/fun/',i))

# settings (with or without adjusting for area)
settings <- list()
settings[[1]] <- list(outdir='out/zmb/', area=F, areaAdjust=F, realModel=T, realTotal=F)
settings[[2]] <- list(outdir='out/zmb_area/', area=T, areaAdjust=F, realModel=T, realTotal=T)
settings[[3]] <- list(outdir='out/zmb_areaAdjust/', area=T, areaAdjust=T, realModel=T, realTotal=T)

for(setting in settings){
  
  # create directories
  dir.create('out', showWarnings=F)
  dir.create(setting$outdir, showWarnings=F)
  for(i in sims){
    dir.create(paste0(setting$outdir,i), showWarnings=F)
  }
  
  # data
  dataZMB(indir='in/', outdir=setting$outdir, area=setting$area, seed=seed)
  
  # fit models
  for(i in sims){
    jagsModel(paste0(setting$outdir,i,'/'), areaAdjust=setting$areaAdjust)
  }
  
  # plot models
  plotModelPanel(file=paste0(setting$outdir,'zmb_model.jpg'), 
                 sims=sims, 
                 dir=setting$outdir, 
                 plotReal=setting$realModel,
                 xmax=200)
  
  # plot totals
  plotTotals(dat = plotTotalsData(dir=setting$outdir, plotReal=T),
             file = paste0(setting$outdir,'zmb_totals.jpg'),
             pos.legend='topleft',
             plotReal = setting$realTotal
  )
}





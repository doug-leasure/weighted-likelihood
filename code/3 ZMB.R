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
settings[[1]] <- list(outdir='out/zmb/', areaAdjust=F)
settings[[2]] <- list(outdir='out/zmb_with_areas/', areaAdjust=T)

for(setting in settings){
  
  # output directory
  outdir <- setting$outdir
  
  # adjust for areas?
  areaAdjust <- setting$areaAdjust
  
  # create directories
  dir.create('out', showWarnings=F)
  dir.create(outdir, showWarnings=F)
  for(i in sims){
    dir.create(paste0(outdir,i), showWarnings=F)
  }
  
  # data
  dataZMB(indir='in/', outdir=outdir, areaAdjust=areaAdjust, seed=seed)
  
  # fit models
  for(i in sims){
    jagsModel(paste0(outdir,i,'/'), areaAdjust=areaAdjust)
  }
  
  # plot models
  plotModelPanel(file=paste0(outdir,'zmb_model.jpg'), 
                 sims=sims, 
                 dir=outdir, 
                 plotReal=F,
                 xmax=300)
  
  # plot totals
  plotTotals(dat = plotTotalsData(dir=outdir),
             file = paste0(outdir,'zmb_totals.jpg'),
             pos.legend='topleft'
  )
}





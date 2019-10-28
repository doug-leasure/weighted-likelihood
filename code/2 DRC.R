# cleanup
rm(list=ls())
gc()
cat("\014") 
try(dev.off())

# seed
seed <- runif(1, 0, 42)
set.seed(seed)

# packages
library(rjags); 
library(dplyr); library(sf); library(reshape2); library(tidyr); library(purrr); library(svDialogs);
library(raster); library(rgdal); library(rgeos); library(tmap); library(tcltk2)

# working directory
setwd('C:/RESEARCH/git/wpgp/weighted-likelihood')
# setwd('//filestore.soton.ac.uk/users/cad1c14/mydocuments/GitHub/weighted-likelihood')

# source functions
for(i in list.files('code/fun') ) source(paste0('code/fun/',i))

# create directories
outdir <- 'out/drc/'
dir.create(outdir, showWarnings=F)

sims <- c('random','weighted_naive','weighted','combined')
for(i in sims){
  dir.create(paste0(outdir,i), showWarnings=F)
}

# data
dataDRC(indir='in/', outdir, seed=seed)

# fit models
for(i in sims){
  jagsModel(paste0(outdir,i,'/'))
}
    
# plot models
plotModelPanel(file=paste0(outdir,'drc_model.jpg'), 
               sims=sims, 
               dir=outdir, 
               plotReal=F)

# plot totals
plotTotals(dat = plotTotalsData(dir=outdir),
           file = paste0(outdir,'drc_totals.jpg'),
           plotReal=F
           )

# map of kinshasa
mapKinshasa(paste0(outdir,'kinshasa_sampling.jpg'))

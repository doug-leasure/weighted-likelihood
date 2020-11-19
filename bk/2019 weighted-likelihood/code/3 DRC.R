# cleanup
rm(list=ls())
gc()
cat("\014")
try(dev.off())

# seed
seed <- runif(1, 0, 42)
set.seed(seed)

# working directory
setwd('C:/RESEARCH/git/wpgp/weighted-likelihood')
# setwd('//filestore.soton.ac.uk/users/cad1c14/mydocuments/GitHub/weighted-likelihood')

# packages
library(devtools)
load_all('pkg')

library(dplyr); library(sf); library(reshape2); library(tidyr); library(purrr); library(svDialogs);
library(raster); library(rgdal); library(rgeos); library(tmap); library(tcltk2)

# load inputs
srcdir <- 'worldpop/Projects/WP517763_GRID3/Working/weighted-likelihood/in'
copyInputs(srcdir=srcdir, outdir=file.path(getwd(),'in'), overwrite=F, OS.type=.Platform$OS.type)

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

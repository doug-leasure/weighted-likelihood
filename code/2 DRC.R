# cleanup
rm(list=ls())
gc()
cat("\014") 
try(dev.off())

set.seed(42)

# load packages
library(rjags); library(dplyr); library(sf); library(reshape2); library(tidyr); library(purrr); library(svDialogs)

# working directory
setwd('C:/RESEARCH/2018 GRID3 WorldPop/git/wpgp/weighted-likelihood')
# setwd('//filestore.soton.ac.uk/users/cad1c14/mydocuments/GitHub/weighted-likelihood')

# source functions
funs <- list.files('code/functions') 
for(fun in funs) source(paste0('code/functions/',fun))

# create directories
dir.create('out', showWarnings=F)
dir.create('out/drc', showWarnings=F)
dir.create('out/drc/random', showWarnings=F)
dir.create('out/drc/weighted_naive', showWarnings=F)
dir.create('out/drc/weighted', showWarnings=F)
dir.create('out/drc/combined', showWarnings=F)

# data
source('code/2b data.R')

# fit models
source('code/2c models.R')

# plot models
source('code/2d plot model.R')

# plot totals
source('code/2e plot totals.R')

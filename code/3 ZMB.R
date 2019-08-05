# cleanup
rm(list=ls())
gc()
cat("\014") 
try(dev.off())

set.seed(42)

# load packages
library(runjags); library(rjags); library(dplyr); library(sf); library(reshape2); library(tidyr); library(purrr); library(svDialogs)

# working directory
setwd('C:/RESEARCH/2018 GRID3 WorldPop/git/wpgp/weighted-likelihood')
# setwd('//filestore.soton.ac.uk/users/cad1c14/mydocuments/GitHub/weighted-likelihood')

# source functions
funs <- list.files('code/functions') 
for(fun in funs) source(paste0('code/functions/',fun))

# create directories
dir.create('out', showWarnings=F)
dir.create('out/zmb', showWarnings=F)
dir.create('out/zmb/random', showWarnings=F)
dir.create('out/zmb/weighted_naive', showWarnings=F)
dir.create('out/zmb/weighted', showWarnings=F)
dir.create('out/zmb/combined', showWarnings=F)

# data
source('code/3b data.R')

# fit models
source('code/3c models.R')

# plot models
source('code/3d plot model.R')

# plot totals
source('code/3e plot totals.R')




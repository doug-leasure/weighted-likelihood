# cleanup
rm(list=ls())
gc()
cat("\014") 
try(dev.off())

set.seed(42)

# load packages
library(runjags);library(boot);library(rjags);library(mcmc)

# working directory
setwd('C:/RESEARCH/2018 GRID3 WorldPop/git/wpgp/weighted-likelihood')
# setwd('//filestore.soton.ac.uk/users/cad1c14/mydocuments/GitHub/weighted-likelihood')

# create directories
dir.create('out', showWarnings=F)
dir.create('out/sims', showWarnings=F)

# source functions
funs <- list.files('code/functions') 
for(fun in funs) source(paste0('code/functions/',fun))
source('code/1b sim function.R')

#==== simulations ====#

# random sampling; two settlement types
sim(sampling='random', n.random=1000, n.weighted=0, ntype=2, outdir='sims/random')

# weighted sampling; two settlement types; no model weights
sim(sampling='weighted', n.random=0, n.weighted=1000, ntype=2, model_weights=F, outdir='sims/weighted_naive')

# weighted sampling; two settlement types; with model weights
sim(sampling='weighted', n.random=0, n.weighted=1000, ntype=2, outdir='sims/weighted')

# random sampling and weighted sampling; two settlement types; with model weights
sim(sampling='combined', n.random=500, n.weighted=500, ntype=2, outdir='sims/combined')

#======= plots =====#

# plot (4 panel) of data and model
source('code/1c plot model.R')

# plot of population totals for each model
source('code/1d plot totals.R')

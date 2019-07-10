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

# create directories
dir.create('out', showWarnings=F)
dir.create('out/sims', showWarnings=F)

# source functions
funs <- c('1f inits.R','1f mean_to_median.R','1f sig_to_log.R','1f sim.R','1f weights_calc.R')
for(fun in funs) source(paste0('code/',fun))

#==== simulations ====#

# random sampling; one settlement type
sim(sampling='random', n.random=1000, ntype=1, outdir='sims/1_random_1type')

# random sampling; two settlement types
sim(sampling='random', n.random=1000, ntype=2, outdir='sims/2_random_2types')

# weighted sampling; one settlement type; no model weights
sim(sampling='weighted', n.weighted=1000, ntype=1, model_weights=F, outdir='sims/3_weighted_1type_equalweights')

# weighted sampling; two settlement types; no model weights
sim(sampling='weighted', n.weighted=1000, ntype=2, model_weights=F, outdir='sims/4_weighted_2types_equalweights')

# weighted sampling; one settlement type; with model weights
sim(sampling='weighted', n.weighted=1000, ntype=1, outdir='sims/5_weighted_1type')

# weighted sampling; two settlement types; with model weights
sim(sampling='weighted', n.weighted=1000, ntype=2, outdir='sims/6_weighted_2types')

# random sampling and weighted sampling; one settlement type; with model weights
sim(sampling='combined', n.random=500, n.weighted=500, ntype=1, outdir='sims/7_combined_1type')

# random sampling and weighted sampling; two settlement types; with model weights
sim(sampling='combined', n.random=500, n.weighted=500, ntype=2, outdir='sims/8_combined_2types')

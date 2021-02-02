# cleanup
rm(list=ls());gc();cat("\014");try(dev.off())

# packages
library(rstan)
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)

# patch for stan glitch: https://discourse.mc-stan.org/t/new-error-cleanup-makevar-old-argument-rmu-is-missing-with-no-default/18137/21
file.rename("~/.R/Makevars.win", "~/.R/Makevars.win.bak")

# working directory
setwd(file.path(dirname(rstudioapi::getSourceEditorContext()$path), '../wd'))

# random seed
seed <- 4242
set.seed(seed)

##------------- user inputs -----------##

# simulated population
n <- 1e6    # total number of population units (e.g. 1 ha grid cells)
med <- 200  # median population per unit
sd <- 0.5   # standard deviation of population among units (log scale)

# simulated sample
n_sample <- 5e3     # total sample size
prop_random <- 0.5  # proportion of sample that is random (for combined sample only)

# models
weighted_model <- '../scripts/models/weighted_likelihood.stan' 
unweighted_model <- '../scripts/models/unweighted.stan'

# output directory
outdir <- 'out_likelihood'

##--------------------------------------##

# output folders
dir.create(file.path(outdir,'obj'), showWarnings=F, recursive=T)
dir.create(file.path(outdir,'fig'), showWarnings=F, recursive=T)

# save settings
saveRDS(list(n=n, med=med, sd=sd, n_sample=n_sample, prop_random=prop_random, 
             weighted_model=weighted_model, unweighted_model=unweighted_model, 
             outdir=outdir, wd=getwd()), 
        file = file.path(outdir,'obj/settings.rds'))


##-------- SIMULATED POPULATION --------##

# population count per unit
pop <- rlnorm(n, log(med), sd)

# simulated population
hist(pop)

# save
saveRDS(pop, file=file.path(outdir,'obj/pop.rds'))



##-------- SAMPLES ---------##


##-- random sample --##

# draw weighted sample
i_sample <- sample(x = 1:n,
                   size = n_sample)

# save sample
pop_sample_random <- pop[i_sample]

# cleanup
rm(i_sample)


##-- weighted sample --##

# sampling weights
weights <- pop / sum(pop)

# draw weighted sample
i_sample <- sample(x = 1:n,
                   size = n_sample,
                   prob = weights)

# save sample
pop_sample_weighted <- pop[i_sample]
weights_sample_weighted <- weights[i_sample]

# cleanup
rm(i_sample)


##-- combined sample --##

# sample sizes
n_sample_random <- round(n_sample * prop_random)
n_sample_weighted <- n_sample - n_sample_random

# draw random sample
i_sample_random <- sample(x = 1:n,
                          size = n_sample_random)
# draw weighted sample
i_sample_weighted <- sample(x = 1:n,
                   size = n_sample_weighted,
                   prob = weights)

# save sample
pop_sample_combo <- pop[c(i_sample_random, i_sample_weighted)]
weights_sample_combo <- c(rep(mean(weights[i_sample_weighted]), n_sample_random), 
                          weights[i_sample_weighted])

# cleanup
rm(i_sample_random, i_sample_weighted)


##-------- MODELS ---------##


##-- model setup --##

# mcmc
pars <- c('med','sigma')
chains <- 4
warmup <- 500
iter <- 1000

# initials
init <- function(c){
  result <- list()
  for(i in 1:c){
    result[[i]] <- list(med = runif(1, 50, 500),
                        sigma = runif(1, 0, 0.5))
  }
  return(result)
}
inits <- init(chains)


##-- random sample / unweighted model --##

# model data
md <- list(N = pop_sample_random,
           n = length(pop_sample_random),
           seed = seed)

# fit
fit <- rstan::stan(file = unweighted_model,
                   data = md,
                   chains = chains,
                   iter = warmup + iter,
                   warmup = warmup,
                   pars = pars,
                   init = inits,
                   seed = md$seed)

# trace plots
rstan::traceplot(fit)

# predictions
df <- as.data.frame(fit)
hat <- rlnorm(nrow(df), log(df$med), df$sigma)

# save
saveRDS(md,file.path(outdir,'obj/md_random_unweighted.rds'))
saveRDS(fit,file.path(outdir,'obj/fit_random_unweighted.rds'))
saveRDS(hat,file.path(outdir,'obj/hat_random_unweighted.rds'))

##-- weighted sample / unweighted model --##

# model data
md <- list(N = pop_sample_weighted,
           n = length(pop_sample_weighted),
           seed = seed)

# fit
fit <- rstan::stan(file = unweighted_model,
                   data = md,
                   chains = chains,
                   iter = warmup + iter,
                   warmup = warmup,
                   pars = pars,
                   init = inits,
                   seed = md$seed)

# trace plots
rstan::traceplot(fit)

# predictions
df <- as.data.frame(fit)
hat <- rlnorm(nrow(df), log(df$med), df$sigma)

# save
saveRDS(md,file.path(outdir,'obj/md_weighted_unweighted.rds'))
saveRDS(fit,file.path(outdir,'obj/fit_weighted_unweighted.rds'))
saveRDS(hat,file.path(outdir,'obj/hat_weighted_unweighted.rds'))

##-- weighted sample / weighted model --##

# model data
md <- list(N = pop_sample_weighted,
           w = weights_sample_weighted,
           n = length(pop_sample_weighted),
           seed = seed)

# fit
fit <- rstan::stan(file = weighted_model,
                   data = md,
                   chains = chains,
                   iter = warmup + iter,
                   warmup = warmup,
                   pars = pars,
                   init = inits,
                   seed = md$seed)

# trace plots
rstan::traceplot(fit)

# predictions
df <- as.data.frame(fit)
hat <- rlnorm(nrow(df), log(df$med), df$sigma)

# save
saveRDS(md,file.path(outdir,'obj/md_weighted_weighted.rds'))
saveRDS(fit,file.path(outdir,'obj/fit_weighted_weighted.rds'))
saveRDS(hat,file.path(outdir,'obj/hat_weighted_weighted.rds'))


##-- combo sample / weighted model --##

# model data
md <- list(N = pop_sample_combo,
           w = weights_sample_combo,
           n = length(pop_sample_combo),
           seed = seed)

# fit
fit <- rstan::stan(file = weighted_model,
                   data = md,
                   chains = chains,
                   iter = warmup + iter,
                   warmup = warmup,
                   pars = pars,
                   init = inits,
                   seed = md$seed)

# trace plots
rstan::traceplot(fit)

# predictions
df <- as.data.frame(fit)
hat <- rlnorm(nrow(df), log(df$med), df$sigma)

# save
saveRDS(md,file.path(outdir,'obj/md_combo_weighted.rds'))
saveRDS(fit,file.path(outdir,'obj/fit_combo_weighted.rds'))
saveRDS(hat,file.path(outdir,'obj/hat_combo_weighted.rds'))

# cleanup
rm(df, hat, fit)


##-------- PLOTS ---------##

# function: axis limits
lim <- function(x){
  xlim <- range(x[[1]]$x)
  for(i in 2:length(x)){
    rng <- range(x[[i]]$x)
    if(rng[1] < xlim[1]) xlim[1] <- rng[1]
    if(rng[2] > xlim[2]) xlim[2] <- rng[2]
  }
  
  # ylim
  ylim <- range(x[[1]]$y)
  for(i in 2:length(x)){
    rng <- range(x[[i]]$y)
    if(rng[1] < ylim[1]) ylim[1] <- rng[1]
    if(rng[2] > ylim[2]) ylim[2] <- rng[2]
  }
  
  return(list(xlim=xlim, ylim=ylim))
}


##-- parameters --##

# image file
jpeg(file.path(outdir,'fig/parameters.jpg'), res=300, height=6, width=6, units='in')

# load data
df <- list(ru = as.data.frame(readRDS(file.path(outdir,'obj/fit_random_unweighted.rds'))),
           wu = as.data.frame(readRDS(file.path(outdir,'obj/fit_weighted_unweighted.rds'))),
           ww = as.data.frame(readRDS(file.path(outdir,'obj/fit_weighted_weighted.rds'))),
           cw = as.data.frame(readRDS(file.path(outdir,'obj/fit_combo_weighted.rds'))))

# panel layout
layout(matrix(1:2, nrow=2, ncol=1, byrow=F), widths=c(1), heights=c(1,1))

## panel 1: median 
par(mar=c(4.5,4.5,1,1))

# density
d <- list(ru = density(df[['ru']]$med),
          wu = density(df[['wu']]$med),
          ww = density(df[['ww']]$med),
          cw = density(df[['cw']]$med))

# plot
l <- lim(d)

plot(NA, 
     main = NULL, 
     ylab = 'Probability Density', 
     xlab = 'Median', 
     xlim = l$xlim, 
     ylim = l$ylim)

abline(v=med, lty=1, lwd=2, col='grey')

lines(d[['ru']], lty=1)
lines(d[['wu']], lty=2)
lines(d[['ww']], lty=3)
lines(d[['cw']], lty=4)

legend('topright', 
       legend = c('true','ru','wu','ww','cw'),
       lty = c(1,1,2,3,4),
       lwd = c(2,1,1,1,1),
       col = c('grey',rep('black',4)))

## panel 2: sigma 
par(mar=c(4.5,4.5,1,1))

# density
d <- list(ru = density(df[['ru']]$sigma),
          wu = density(df[['wu']]$sigma),
          ww = density(df[['ww']]$sigma),
          cw = density(df[['cw']]$sigma))

# plot
l <- lim(d)

plot(NA, 
     main = NULL, 
     ylab = 'Probability Density', 
     xlab = 'Standard Deviation', 
     xlim = l$xlim, 
     ylim = l$ylim)

abline(v=sd, lwd=2, col='grey')

lines(d[['ru']], lty=1)
lines(d[['wu']], lty=2)
lines(d[['ww']], lty=3)
lines(d[['cw']], lty=4)

# save
dev.off()









##-- distribution --##



##-- totals --##









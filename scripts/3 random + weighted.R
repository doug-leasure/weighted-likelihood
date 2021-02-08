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
med <- 250  # median population per unit
sigma <- 0.5   # standard deviation of population among units (log scale)

# simulated sample
n_sample <- 2e3     # total sample size
prop_random <- 0.5  # proportion of sample that is random (for combined sample only)

# models
weighted_model <- '../models/weighted_precision.stan' 
precision <- T

unweighted_model <- '../models/unweighted.stan'

##--------------------------------------##

# output directory
outdir <- file.path('random + weighted',
                    paste0(ifelse(precision,'precision','likelihood'),'_med',med,'sigma',sigma,'prop',prop_random))
dir.create(outdir, recursive=T)

# save settings
saveRDS(list(n=n, med=med, sigma=sigma, n_sample=n_sample, prop_random=prop_random, 
             weighted_model=weighted_model, unweighted_model=unweighted_model, 
             outdir=outdir, wd=getwd()), 
        file = file.path(outdir,'settings.rds'))


##-------- SIMULATED POPULATION --------##

# population count per unit
pop <- rlnorm(n, log(med), sigma)

# simulated population
hist(pop)

# save
saveRDS(pop, file=file.path(outdir,'pop.rds'))


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
warmup <- 1000
iter <- 1000

# function: initials
init <- function(c, d, med, sigma){
  result <- list()
  for(i in 1:c){
    result[[i]] <- list(med = runif(1, med/2, min(1e3,med*1.5)),
                        sigma = runif(1, sigma/2, min(1,sigma*1.5)))
    
    result[[i]]$theta <- sqrt( mean(d$w) / result[[i]]$sigma^-2 )
  }
  return(result)
}

# function: remove parameters from initials
rm_init <- function(rm_names, init_list){
  for(i in 1:length(init_list)){
    init_list[[i]] <- init_list[[i]][-which(names(init_list[[i]]) %in% rm_names)]
  }
  return(init_list)
}


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
                   init = rm_init('theta', init(chains, md, med, sigma)),
                   seed = md$seed)

# trace plots
jpeg(file.path(outdir, 'trace_random_unweighted.jpg'))
  print(rstan::traceplot(fit))
dev.off()

# predictions
df <- as.data.frame(fit)
hat <- rlnorm(nrow(df), log(df$med), df$sigma)

# save
saveRDS(md,file.path(outdir,'md_random_unweighted.rds'))
saveRDS(fit,file.path(outdir,'fit_random_unweighted.rds'))
saveRDS(hat,file.path(outdir,'hat_random_unweighted.rds'))

# cleanup
rm(fit,hat,df,md)

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
                   init = rm_init('theta', init(chains, md, med, sigma)),
                   seed = md$seed)

# trace plots
jpeg(file.path(outdir, 'trace_weighted_unweighted.jpg'))
  print(rstan::traceplot(fit))
dev.off()

# predictions
df <- as.data.frame(fit)
hat <- rlnorm(nrow(df), log(df$med), df$sigma)

# save
saveRDS(md,file.path(outdir,'md_weighted_unweighted.rds'))
saveRDS(fit,file.path(outdir,'fit_weighted_unweighted.rds'))
saveRDS(hat,file.path(outdir,'hat_weighted_unweighted.rds'))

# cleanup
rm(fit,hat,df,md)

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
                   init = rm_init(ifelse(precision,'sigma','theta') , init(chains, md, med, sigma)),
                   seed = md$seed)

# trace plots
jpeg(file.path(outdir, 'trace_weighted_weighted.jpg'))
print(rstan::traceplot(fit))
dev.off()

# predictions
df <- as.data.frame(fit)
hat <- rlnorm(nrow(df), log(df$med), df$sigma)

# save
saveRDS(md,file.path(outdir,'md_weighted_weighted.rds'))
saveRDS(fit,file.path(outdir,'fit_weighted_weighted.rds'))
saveRDS(hat,file.path(outdir,'hat_weighted_weighted.rds'))

# cleanup
rm(fit,hat,df,md)

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
                   init = rm_init(ifelse(precision,'sigma','theta') , init(chains, md, med, sigma)),
                   seed = md$seed)

# trace plots
jpeg(file.path(outdir, 'trace_combo_weighted.jpg'))
  print(rstan::traceplot(fit))
dev.off()


# predictions
df <- as.data.frame(fit)
hat <- rlnorm(nrow(df), log(df$med), df$sigma)

# save
saveRDS(md,file.path(outdir,'md_combo_weighted.rds'))
saveRDS(fit,file.path(outdir,'fit_combo_weighted.rds'))
saveRDS(hat,file.path(outdir,'hat_combo_weighted.rds'))

# cleanup
rm(fit,hat,df,md)


##-------- PLOT 1: PARAMETERS ---------####

# load data
df <- list(ru = as.data.frame(readRDS(file.path(outdir,'fit_random_unweighted.rds'))),
           wu = as.data.frame(readRDS(file.path(outdir,'fit_weighted_unweighted.rds'))),
           ww = as.data.frame(readRDS(file.path(outdir,'fit_weighted_weighted.rds'))),
           cw = as.data.frame(readRDS(file.path(outdir,'fit_combo_weighted.rds'))))

md <- list(ru = readRDS(file.path(outdir,'md_random_unweighted.rds')),
           wu = readRDS(file.path(outdir,'md_weighted_unweighted.rds')),
           ww = readRDS(file.path(outdir,'md_weighted_weighted.rds')),
           cw = readRDS(file.path(outdir,'md_combo_weighted.rds')))

# tables of parameter estimates
median_table <- 
  mean_table <- 
  sigma_table <- data.frame(row.names=c('pop','ru','wu','ww','cw'))

median_table[,c('median','mean','lower','upper')] <- 
  mean_table[,c('median','mean','lower','upper')] <- 
  sigma_table[,c('median','mean','lower','upper')] <- NA

median_table['pop',] <- med
mean_table['pop',] <- med*exp(0.5*sigma^2)
sigma_table['pop',] <- sigma

for(name in names(df)){
  median_table[name,'median'] <- median(df[[name]]$med)
  median_table[name,'mean'] <- mean(df[[name]]$med)
  median_table[name,c('lower','upper')] <- quantile(df[[name]]$med, probs=c(0.025,0.975))

  mean_table[name,'median'] <- median(df[[name]]$med * exp(0.5*df[[name]]$sigma^2))
  mean_table[name,'mean'] <- mean(df[[name]]$med * exp(0.5*df[[name]]$sigma^2))
  mean_table[name,c('lower','upper')] <- quantile(df[[name]]$med * exp(0.5*df[[name]]$sigma^2), 
                                                  probs=c(0.025,0.975))
  
  sigma_table[name,'median'] <- median(df[[name]]$sigma)
  sigma_table[name,'mean'] <- mean(df[[name]]$sigma)
  sigma_table[name,c('lower','upper')] <- quantile(df[[name]]$sigma, probs=c(0.025,0.975))
  
}

# save tables
write.csv(median_table, file=file.path(outdir, 'median_table.csv'))
write.csv(mean_table, file=file.path(outdir, 'mean_table.csv'))
write.csv(sigma_table, file=file.path(outdir, 'sigma_table.csv'))

# function: axis limits
lim <- function(x, prec=precision){
  xlim <- range(x[[1]]$x)
  for(i in 2:length(x)){
    rng <- range(x[[i]]$x)
    if(rng[1] < xlim[1]) xlim[1] <- rng[1]
    if(rng[2] > xlim[2]) xlim[2] <- rng[2]
  }

  # ylim
  ylim <- range(x[[1]]$y)
  nmax_ylim <- ifelse(prec,length(x),2)
  for(i in 2:nmax_ylim){
    rng <- range(x[[i]]$y)
    if(rng[1] < ylim[1]) ylim[1] <- rng[1]
    if(rng[2] > ylim[2]) ylim[2] <- rng[2]
  }

  return(list(xlim=xlim, ylim=ylim))
}

# image file
jpeg(file.path(outdir,'parameters.jpg'), res=300, height=7, width=5, units='in')

# panel layout
layout(matrix(1:3, nrow=3, ncol=1, byrow=F), widths=c(1), heights=c(1,1,1))

## panel 1: median 
par(mar=c(4.5,4.5,1,1))

# density
d <- list(ru = density(df[['ru']]$med),
          wu = density(df[['wu']]$med),
          ww = density(df[['ww']]$med),
          cw = density(df[['cw']]$med))

# plot
l <- lim(d, precision)
l$xlim[2] <- l$xlim[2] * 1.05

plot(NA, 
     main = NULL, 
     ylab = 'Probability Density', 
     xlab = expression(paste('Median ', mu)), 
     xlim = l$xlim, 
     ylim = l$ylim)

lines(d[['ru']], lty=1, lwd=2, col='darkgrey')
lines(d[['wu']], lty=2, lwd=2, col='darkgrey')
lines(d[['ww']], lty=1, lwd=2, col='black')
lines(d[['cw']], lty=2, lwd=2, col='black')

abline(v=med, lty=1, lwd=3, col='red')

legend('topright', 
       legend = c('true','ru','wu','ww','cw'),
       lty = c(1,1,2,1,2),
       lwd = c(3,2,2,2,2),
       col = c('red','darkgrey','darkgrey','black','black'))

## panel 2: mean 
par(mar=c(4.5,4.5,1,1))

# density
d <- list(ru = density(df[['ru']]$med * exp( df[['ru']]$sigma^2 / 2 )),
          wu = density(df[['wu']]$med * exp( df[['wu']]$sigma^2 / 2 )),
          ww = density(df[['ww']]$med * exp( df[['ww']]$sigma^2 / 2 )),
          cw = density(df[['cw']]$med * exp( df[['cw']]$sigma^2 / 2 )))

# plot
l <- lim(d, precision)

plot(NA, 
     main = NULL, 
     ylab = 'Probability Density', 
     xlab = expression(paste('Mean ', mu, e^(0.5 * sigma^2))), 
     xlim = l$xlim, 
     ylim = l$ylim)

lines(d[['ru']], lty=1, lwd=2, col='darkgrey')
lines(d[['wu']], lty=2, lwd=2, col='darkgrey')
lines(d[['ww']], lty=1, lwd=2, col='black')
lines(d[['cw']], lty=2, lwd=2, col='black')

abline(v=med*exp(sigma^2/2), lty=1, lwd=3, col='red')

## panel 3: sigma 
par(mar=c(4.5,4.5,1,1))

# density
d <- list(ru = density(df[['ru']]$sigma),
          wu = density(df[['wu']]$sigma),
          ww = density(df[['ww']]$sigma),
          cw = density(df[['cw']]$sigma))

# plot
l <- lim(d, precision)

plot(NA, 
     main = NULL, 
     ylab = 'Probability Density', 
     xlab = expression(paste('Standard Deviation ', sigma)), 
     xlim = l$xlim, 
     ylim = l$ylim)

lines(d[['ru']], lty=1, lwd=2, col='darkgrey')
lines(d[['wu']], lty=2, lwd=2, col='darkgrey')
lines(d[['ww']], lty=1, lwd=2, col='black')
lines(d[['cw']], lty=2, lwd=2, col='black')

abline(v=sigma, lty=1, lwd=3, col='red')

# save
dev.off()







##-------- PLOT 2: DISTRIBUTIONS ---------####

# image file
jpeg(file.path(outdir,'distributions.jpg'), res=600, height=7, width=7, units='in')

# load data
df <- list(ru = readRDS(file.path(outdir,'hat_random_unweighted.rds')),
           wu = readRDS(file.path(outdir,'hat_weighted_unweighted.rds')),
           ww = readRDS(file.path(outdir,'hat_weighted_weighted.rds')),
           cw = readRDS(file.path(outdir,'hat_combo_weighted.rds')))

# density
d <- list(pop = density(pop),
          rdat = density(pop_sample_random),
          wdat = density(pop_sample_weighted),
          cdat = density(pop_sample_combo),
          ru = density(df[['ru']]),
          wu = density(df[['wu']]),
          ww = density(df[['ww']]),
          cw = density(df[['cw']]))

# panel layout
layout(matrix(1:4, nrow=2, ncol=2, byrow=T), widths=c(1,1), heights=c(1,1))

##-- panel 1: random-unweighted --##
par(mar=c(2.5,4.5,1,1))

# plot
l <- lim(d, precision)

plot(NA, 
     main = NULL, 
     ylab = 'Probability Density', 
     xlab = NULL, 
     xlim = c(0,max(pop_sample_weighted)), 
     ylim = l$ylim)

polygon(y=c(d[['pop']]$y, rep(0,length(d[['pop']]$y))),
        x=c(d[['pop']]$x, rev(d[['pop']]$x)) , col=gray(0.6, alpha=0.5), border=NA)

lines(d[['rdat']], lty=3, col='black')
lines(d[['ru']], lty=1, col='black')

legend('topright',
       legend=c('population','random sample','unweighted model'),
       fill=c(gray(0.6),NA,NA),
       border=c(NA,NA,NA),
       lty = c(NA,3,1),
       lwd = c(NA,1,1))

##-- panel 2: weighted-unweighted --##
par(mar=c(2.5,2.5,1,1))

# plot
l <- lim(d, precision)

plot(NA, 
     main = NULL, 
     ylab = NULL, 
     xlab = NULL, 
     xlim = c(0,max(pop_sample_weighted)), 
     ylim = l$ylim)

polygon(y=c(d[['pop']]$y, rep(0,length(d[['pop']]$y))),
        x=c(d[['pop']]$x, rev(d[['pop']]$x)) , col=gray(0.6, alpha=0.5), border=NA)

lines(d[['wdat']], lty=3, col='black')
lines(d[['wu']], lty=1, col='black')

legend('topright',
       legend=c('population','weighted sample','unweighted model'),
       fill=c(gray(0.6),NA,NA),
       border=c(NA,NA,NA),
       lty = c(NA,3,1),
       lwd = c(NA,1,1))

##-- panel 3: weighted-weighted --##
par(mar=c(4.5,4.5,1,1))

# plot
l <- lim(d, precision)

plot(NA, 
     main = NULL, 
     ylab = 'Probability Density', 
     xlab = 'Value', 
     xlim = c(0,max(pop_sample_weighted)), 
     ylim = l$ylim)

polygon(y=c(d[['pop']]$y, rep(0,length(d[['pop']]$y))),
        x=c(d[['pop']]$x, rev(d[['pop']]$x)) , col=gray(0.6, alpha=0.5), border=NA)

lines(d[['wdat']], lty=3, col='black')
lines(d[['ww']], lty=1, col='black')

legend('topright',
       legend=c('population','weighted sample','weighted model'),
       fill=c(gray(0.6),NA,NA),
       border=c(NA,NA,NA),
       lty = c(NA,3,1),
       lwd = c(NA,1,1))

##-- panel 4: combo-weighted --##
par(mar=c(4.5,2.5,1,1))

# plot
l <- lim(d, precision)

plot(NA, 
     main = NULL, 
     ylab = NULL, 
     xlab = 'Value', 
     xlim = c(0,max(pop_sample_weighted)), 
     ylim = l$ylim)

polygon(y=c(d[['pop']]$y, rep(0,length(d[['pop']]$y))),
        x=c(d[['pop']]$x, rev(d[['pop']]$x)) , col=gray(0.6, alpha=0.5), border=NA)

lines(d[['cdat']], lty=3, col='black')
lines(d[['cw']], lty=1, col='black')

legend('topright',
       legend=c('population','combo sample','weighted model'),
       fill=c(gray(0.6),NA,NA),
       border=c(NA,NA,NA),
       lty = c(NA,3,1),
       lwd = c(NA,1,1))

dev.off()






##-------- PLOT 3: TOTALS ---------####

# load fitted models
df <- list(ru = as.data.frame(readRDS(file.path(outdir,'fit_random_unweighted.rds'))),
           wu = as.data.frame(readRDS(file.path(outdir,'fit_weighted_unweighted.rds'))),
           ww = as.data.frame(readRDS(file.path(outdir,'fit_weighted_weighted.rds'))),
           cw = as.data.frame(readRDS(file.path(outdir,'fit_combo_weighted.rds'))))

# prepare plot data
totals <- data.frame(row.names=c('pop','ru','wu','ww','cw'))
totals[,c('median','mean','lower','upper')] <- NA
totals['pop',] <- sum(pop)

### posterior sums ### SLOW
for(mod in c('ru','wu','ww','cw')){
  tot <- rep(0, nrow(df[[mod]]))
  for(i in 1:n){
    tot <- tot + rlnorm(nrow(df[[mod]]), log(df[[mod]]$med), df[[mod]]$sigma)
  }
  totals[mod,'mean'] <- mean(tot)
  totals[mod,'median'] <- median(tot)
  totals[mod,'lower'] <- quantile(tot, probs=0.025)
  totals[mod,'upper'] <- quantile(tot, probs=0.975)
  print(totals)
}

# save plot data
write.csv(totals, file=file.path(outdir,'totals.csv'))

###################

# image file
jpeg(file.path(outdir,'totals.jpg'), res=600, height=6, width=6, units='in')

# plot
bd <- as.matrix(totals[c('ru','wu','ww','cw'),])
rownames(bd) <- rownames(totals)[-1]
colnames(bd) <- colnames(totals)

bp <- barplot(height=bd[,'mean'],
              main=NA,
              beside=T,
              ylim=c(0,max(bd[,'upper'],na.rm=T)*1.1),
              ylab='Population Total')

abline(h=totals['pop','mean'])

row.names(bp) <- row.names(totals)[-1]
colnames(bp) <- 'x'

# add error bars and real values (if known)
for(i in row.names(bp)){
  arrows(x0 = bp[i,1],
         y0 = totals[i,'lower'],
         y1 = totals[i,'upper'],
         length = 0.1, angle=90, lwd=1, code=3
  )
}
dev.off()






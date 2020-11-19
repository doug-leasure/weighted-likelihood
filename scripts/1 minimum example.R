# cleanup
rm(list=ls());gc();cat("\014");try(dev.off())

# packages
library(rstan)
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)

# patch for stan glitch: https://discourse.mc-stan.org/t/new-error-cleanup-makevar-old-argument-rmu-is-missing-with-no-default/18137/21
file.rename("~/.R/Makevars.win", "~/.R/Makevars.win.bak")

# working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# simulation parameters
med <- 200
sd <- 0.5

# population count per 100 m pixel
population <- rlnorm(1e6, log(med), sd)

# simulated population
hist(population)

# sampling weights
weights <- population
weights <- weights / sum(weights)

# draw sample
i_sample <- sample(x = 1:length(population),
                   size = 1e3,
                   prob = weights)

pop_sample <- population[i_sample]

weights_sample <- weights[i_sample]

# compare true population to weighted sample
median(population)
median(pop_sample)

mean(population)
mean(pop_sample)

sd(population)
sd(pop_sample)

# model data
md <- list(N = pop_sample,
           w = weights_sample,
           n = length(pop_sample),
           seed = runif(1,1,4242))

# mcmc
pars <- c('med','theta','sigma_bar')
chains <- 4
warmup <- 500
iter <- 1000

fit <- rstan::stan(file = 'models/model_A.stan',
                   data = md,
                   chains = chains,
                   iter = warmup + iter,
                   warmup = warmup,
                   pars = pars,
                   seed = md$seed)

# trace plots
rstan::traceplot(fit)

# fit to df
df <- as.data.frame(fit)

# predictions
Nhat <- rlnorm(nrow(df), log(df$med), df$sigma_bar)

## compare results: true population, sample data, model estimate

# median
median(population)
median(pop_sample)
median(Nhat)

# mean
mean(population)
mean(pop_sample)
mean(Nhat)

# sd
sd(population)
sd(pop_sample)
sd(Nhat)








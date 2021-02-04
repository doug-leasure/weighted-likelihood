# cleanup
rm(list=ls());gc();cat("\014");try(dev.off())

# packages
library(rstan);library(runjags)
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE)

# # patch for stan glitch: https://discourse.mc-stan.org/t/new-error-cleanup-makevar-old-argument-rmu-is-missing-with-no-default/18137/21
# file.rename("~/.R/Makevars.win", "~/.R/Makevars.win.bak")

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
pars <- c('med','sigma')
chains <- 4
warmup <- 1000
iter <- 1000

# initials
init <- function(c, d){
  result <- list()
  for(i in 1:c){
    result[[i]] <- list(med = runif(1, 50, 500),
                        sigma = runif(1, 0, 0.5))
    result[[i]]$theta <- sqrt( mean(d$w) / result[[i]]$sigma^-2 )
    result[[i]]$.RNG.name = "lecuyer::RngStream" #"base::Wichmann-Hill"
    result[[i]]$.RNG.seed = md$seed + c
  }
  return(result)
}
rm_init <- function(rm_names, init_list){
  for(i in 1:length(init_list)){
    init_list[[i]] <- init_list[[i]][-which(names(init_list[[i]]) %in% rm_names)]
  }
  return(init_list)
}
inits <- init(chains, md)

##--------------##

# fit Stan weighted likelihood model
fitA <- rstan::stan(file = 'models/weighted_likelihood.stan',
                   data = md,
                   chains = chains,
                   iter = warmup + iter,
                   warmup = warmup,
                   pars = pars,
                   init = rm_init(c('theta','.RNG.name','.RNG.seed'),inits),
                   seed = md$seed)

# trace plots
rstan::traceplot(fitA)

# fit to df
dfA <- as.data.frame(fitA)

# predictions
NhatA <- rlnorm(nrow(dfA), log(dfA$med), dfA$sigma)

##--------------##

# fit Stan weighted precision model
fitB <- rstan::stan(file = 'models/weighted_precision.stan',
                    data = md,
                    chains = chains,
                    iter = warmup + iter,
                    warmup = warmup,
                    pars = pars,
                    init = rm_init(c('sigma','.RNG.name','.RNG.seed'),inits),
                    seed = md$seed)

# trace plots
rstan::traceplot(fitB)

# fit to df
dfB <- as.data.frame(fitB)

# predictions
NhatB <- rlnorm(nrow(dfB), log(dfB$med), dfB$sigma)

##--------------##

# fit JAGS weighted precision model
fitC <- run.jags(model = 'models/weighted_precision.jags',
                 data = md,
                 n.chains = chains,
                 sample = iter,
                 burnin = warmup,
                 monitor = pars,
                 inits = rm_init('sigma',inits),
                 thin = 1,
                 summarise = F,
                 method = 'parallel'
  )

# trace plots
coda::traceplot(fitC$mcmc)

# fit to df
dfC <- as.data.frame(fitC$mcmc[[1]])
for(i in 2:length(fitC$mcmc)){
  dfC <- rbind(dfC, fitC$mcmc[[i]])
}

# predictions
NhatC <- rlnorm(nrow(dfC), log(dfC$med), dfC$sigma)

##--------------##

## compare results: true population, sample data, model estimate

# median
median(population)
median(pop_sample)
median(NhatA)
median(NhatB)
median(NhatC)

# mean
mean(population)
mean(pop_sample)
mean(NhatA)
mean(NhatB)
mean(NhatC)

# sd
sd(population)
sd(pop_sample)
sd(NhatA)
sd(NhatB)
sd(NhatC)








# cleanup
rm(list=ls());gc();cat("\014");try(dev.off())

# working directory
setwd(file.path(dirname(rstudioapi::getSourceEditorContext()$path), '../wd'))

# random seed
seed <- 4242
set.seed(seed)

# population parameters
lambda <- 5

# number of locations in full population
n_pop <- 1e6

# simulate population densities at all locations
pop <- rpois(n = n_pop,
             lambda = lambda)

# plot distribution of population densities
hist(pop)

# sample size (number of locations)
n_sample <- 2e3

# sampling weight based on population counts
w <- (pop+1) / sum(pop+1)

# select locations for a weighted sample
i <- sample(x = 1:n_pop,
            size = n_sample,
            prob = w)

# model weight
m_i <- w[i]^(-1) / sum(w[i]^(-1))

# comparison
mean( pop )         # true pop
mean( pop[i] )      # sample
sum( pop[i] * m_i ) # weighted average












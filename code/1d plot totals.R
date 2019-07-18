# image file
jpeg('out/sim_totals.jpg', res=300, height=6, width=6, units='in')

# plot function
source('code/functions/plotTotals.R')

# load data
d_random <- read.csv('out/sims/random/d.csv', check.names=F)
d_combined <- read.csv('out/sims/combined/d.csv', check.names=F)
d_weighted_naive <- read.csv('out/sims/weighted_naive/d.csv', check.names=F)
d_weighted <- read.csv('out/sims/weighted/d.csv', check.names=F)

real1_random <- readRDS('out/sims/random/real1.rds')
real2_random <- readRDS('out/sims/random/real2.rds')
real1_weighted_naive <- readRDS('out/sims/weighted_naive/real1.rds')
real2_weighted_naive <- readRDS('out/sims/weighted_naive/real2.rds')
real1_weighted <- readRDS('out/sims/weighted/real1.rds')
real2_weighted <- readRDS('out/sims/weighted/real2.rds')
real1_combined <- readRDS('out/sims/combined/real1.rds')
real2_combined <- readRDS('out/sims/combined/real2.rds')

# setup output data
real <- totals <- upper <- lower <- data.frame(Urban=rep(NA, 4),
                                               Rural=rep(NA, 4),
                                               row.names=c('Random','Weighted Unadjusted','Weighted Adjusted','Combined'))


# real totals
real['Random','Urban'] <- sum(real1_random)
real['Weighted Unadjusted','Urban'] <- sum(real1_weighted_naive)
real['Weighted Adjusted','Urban'] <- sum(real1_weighted)
real['Combined','Urban'] <- sum(real1_combined)

real['Random','Rural'] <- sum(real2_random)
real['Weighted Unadjusted','Rural'] <- sum(real2_weighted_naive)
real['Weighted Adjusted','Rural'] <- sum(real2_weighted)
real['Combined','Rural'] <- sum(real2_combined)

# estimated totals
totals['Random','Urban'] <- mean(d_random$`poptotal[1]`)
totals['Weighted Unadjusted','Urban'] <- mean(d_weighted_naive$`poptotal[1]`)
totals['Weighted Adjusted','Urban'] <- mean(d_weighted$`poptotal[1]`)
totals['Combined','Urban'] <- mean(d_combined$`poptotal[1]`)

totals['Random','Rural'] <- mean(d_random$`poptotal[2]`)
totals['Weighted Unadjusted','Rural'] <- mean(d_weighted_naive$`poptotal[2]`)
totals['Weighted Adjusted','Rural'] <- mean(d_weighted$`poptotal[2]`)
totals['Combined','Rural'] <- mean(d_combined$`poptotal[2]`)

# upper
upper['Random','Urban'] <- quantile(d_random$`poptotal[1]`, probs=c(0.975))
upper['Weighted Unadjusted','Urban'] <- quantile(d_weighted_naive$`poptotal[1]`, probs=c(0.975))
upper['Weighted Adjusted','Urban'] <- quantile(d_weighted$`poptotal[1]`, probs=c(0.975))
upper['Combined','Urban'] <- quantile(d_combined$`poptotal[1]`, probs=c(0.975))

upper['Random','Rural'] <- quantile(d_random$`poptotal[2]`, probs=c(0.975))
upper['Weighted Unadjusted','Rural'] <- quantile(d_weighted_naive$`poptotal[2]`, probs=c(0.975))
upper['Weighted Adjusted','Rural'] <- quantile(d_weighted$`poptotal[2]`, probs=c(0.975))
upper['Combined','Rural'] <- quantile(d_combined$`poptotal[2]`, probs=c(0.975))

# lower
lower['Random','Urban'] <- quantile(d_random$`poptotal[1]`, probs=c(0.025))
lower['Weighted Unadjusted','Urban'] <- quantile(d_weighted_naive$`poptotal[1]`, probs=c(0.025))
lower['Weighted Adjusted','Urban'] <- quantile(d_weighted$`poptotal[1]`, probs=c(0.025))
lower['Combined','Urban'] <- quantile(d_combined$`poptotal[1]`, probs=c(0.025))

lower['Random','Rural'] <- quantile(d_random$`poptotal[2]`, probs=c(0.025))
lower['Weighted Unadjusted','Rural'] <- quantile(d_weighted_naive$`poptotal[2]`, probs=c(0.025))
lower['Weighted Adjusted','Rural'] <- quantile(d_weighted$`poptotal[2]`, probs=c(0.025))
lower['Combined','Rural'] <- quantile(d_combined$`poptotal[2]`, probs=c(0.025))

# plot
plotTotals(main='Simulation Population Totals',
           bardata = as.matrix(totals), 
           lower = lower, 
           upper = upper, 
           real = real
           )

dev.off()



# cleanup
rm(list=ls())
gc()
cat("\014") 
try(dev.off())

set.seed(42)

# working directory
setwd('C:/RESEARCH/2018 GRID3 WorldPop/git/wpgp/weighted-likelihood')

# load data
real1 <- readRDS('out/sims/2_random_2types/real1.rds')
real2 <- readRDS('out/sims/2_random_2types/real2.rds')

d_random <- read.csv('out/sims/2_random_2types/d.csv', check.names=F)
d_combined <- read.csv('out/sims/8_combined_2types/d.csv', check.names=F)
d_weighted_naive <- read.csv('out/sims/4_weighted_2types_equalweights/d.csv', check.names=F)
d_weighted <- read.csv('out/sims/6_weighted_2types/d.csv', check.names=F)

# calculate totals
bardata <- data.frame(Urban=rep(NA,4), Rural=rep(NA,4), row.names=c('Random','Weighted Unadjusted','Weighted Adjusted','Combined'))

bardata['Random','Urban'] <- sum(rlnorm(length(real1), log(mean(d_random$`med[1]`)), mean(d_random$`LOG_SIGMA[1]`) ) )
bardata['Random','Rural'] <- sum(rlnorm(length(real1), log(mean(d_random$`med[2]`)), mean(d_random$`LOG_SIGMA[2]`) ) )
bardata['Weighted Unadjusted','Urban'] <- sum(rlnorm(length(real1), log(mean(d_weighted_naive$`med[1]`)), mean(d_weighted_naive$`LOG_SIGMA[1]`) ) )
bardata['Weighted Unadjusted','Rural'] <- sum(rlnorm(length(real1), log(mean(d_weighted_naive$`med[2]`)), mean(d_weighted_naive$`LOG_SIGMA[2]`) ) )
bardata['Weighted Adjusted','Urban'] <- sum(rlnorm(length(real1), log(mean(d_weighted$`med[1]`)), mean(d_weighted$`LOG_SIGMA[1]`) ) )
bardata['Weighted Adjusted','Rural'] <- sum(rlnorm(length(real1), log(mean(d_weighted$`med[2]`)), mean(d_weighted$`LOG_SIGMA[2]`) ) )
bardata['Combined','Urban'] <- sum(rlnorm(length(real1), log(mean(d_combined$`med[1]`)), mean(d_combined$`LOG_SIGMA[1]`) ) )
bardata['Combined','Rural'] <- sum(rlnorm(length(real1), log(mean(d_combined$`med[2]`)), mean(d_combined$`LOG_SIGMA[2]`) ) )

bardata <- as.matrix(bardata)

total1 <- sum(real1)
total2 <- sum(real2)

# image file
jpeg('manuscript/sim_totals.jpg', res=300, height=6, width=6, units='in')

# plot
bp <- barplot(height=bardata, 
              main='Simulation Population Totals',
              beside=T,
              space=c(0.1, 0.5),
              ylim=c(0, 7.5e6),
              ylab='Population Total',
              legend.text=T,
              args.legend=list(x='topright', bty='n'))

xcut <- mean(c(bp[4,1], bp[1,2]))
arrows(x0=0, x1=xcut, y0=total1, y1=total1, length=0, lty=2)
arrows(x0=xcut, x1=10, y0=total2, y1=total2, length=0, lty=2)

dev.off()



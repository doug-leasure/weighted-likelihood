# cleanup
rm(list=ls())
gc()
cat("\014") 
try(dev.off())

set.seed(42)

# working directory
setwd('C:/RESEARCH/2018 GRID3 WorldPop/git/wpgp/weighted-likelihood')
outdir <- 'out/drc/'

# load data
d_random <- read.csv('out/drc/d_random.csv', check.names=F)
d_weighted <- read.csv('out/drc/d_weighted.csv', check.names=F)
d_weighted_naive <- read.csv('out/drc/d_weighted_naive.csv', check.names=F)
d_all <- read.csv('out/drc/d_all.csv', check.names=F)

# plot: Settlement type 1
jpeg(paste0(outdir,'urban_posterior.jpg'))

density_random <- density(d_random$`yhat[1]`)
density_weighted_naive <- density(d_weighted_naive$`yhat[1]`)
density_weighted <- density(d_weighted$`yhat[1]`)
density_all <- density(d_all$`yhat[1]`)

xlim <- c(0, 1000)
ylim <- c(0, max(c(density_random$y, density_weighted$y, density_weighted_naive$y, density_all$y)))

plot(NA, xlim=xlim, ylim=ylim, main='Urban', xlab='Population', ylab='Probability')

# lines(density_all, col='black', lwd=1, lty=1)
polygon(y=c(density_all$y, rep(0,length(density_all$y))),
        x=c(density_all$x, rev(density_all$x)) , col='lightgrey', border=NA)

lines(density_random, col='black', lwd=2, lty=3)
lines(density_weighted_naive, col='darkgrey', lwd=2, lty=2)
lines(density_weighted, col='black', lwd=2, lty=2)

legend('topright', legend=c('Random','Weighted Unadjusted','Weighted Adjusted','All'),
       col=c('black','darkgrey','black',NA),
       fill=c(NA,NA,NA,'lightgrey'),
       border=c(NA,NA,NA,NA),
       lwd=c(2,2,2,NA),
       lty=c(3,2,2,NA),
       x.intersp=c(2,2,2,1),
       bty='n')

dev.off()

# plot: Settlement type 2
jpeg(paste0(outdir,'rural_posterior.jpg'))

density_random <- density(d_random$`yhat[2]`)
density_weighted_naive <- density(d_weighted_naive$`yhat[2]`)
density_weighted <- density(d_weighted$`yhat[2]`)
density_all <- density(d_all$`yhat[2]`)

xlim <- c(0, 1000)
ylim <- c(0, max(c(density_random$y, density_weighted$y, density_weighted_naive$y, density_all$y)))

plot(NA, xlim=xlim, ylim=ylim, main='Rural', xlab='Population', ylab='Probability')

# lines(density_all, col='black', lwd=1, lty=1)
polygon(y=c(density_all$y, rep(0,length(density_all$y))),
        x=c(density_all$x, rev(density_all$x)) , col='lightgrey', border=NA)

lines(density_random, col='black', lwd=2, lty=3)
lines(density_weighted_naive, col='darkgrey', lwd=2, lty=2)
lines(density_weighted, col='black', lwd=2, lty=2)

legend('topright', legend=c('Random','Weighted Unadjusted','Weighted Adjusted','All'),
       col=c('black','darkgrey','black',NA),
       fill=c(NA,NA,NA,'lightgrey'),
       border=c(NA,NA,NA,NA),
       lwd=c(2,2,2,NA),
       lty=c(3,2,2,NA),
       x.intersp=c(2,2,2,1),
       bty='n')

dev.off()

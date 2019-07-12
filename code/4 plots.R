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

# density plot: Settlement type 1
jpeg(paste0(outdir,'urban_posterior.jpg'))

density_random <- density(d_random$`yhat[1]`)
density_weighted_naive <- density(d_weighted_naive$`yhat[1]`)
density_weighted <- density(d_weighted$`yhat[1]`)
density_all <- density(d_all$`yhat[1]`)

xlim <- c(0, 1000)
ylim <- c(0, max(c(density_random$y, density_weighted$y, density_weighted_naive$y, density_all$y)))

plot(NA, xlim=xlim, ylim=ylim, main=paste('Urban','Estimates'), xlab='Population Density', ylab='Probability')

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

# density plot: Settlement type 2
jpeg(paste0(outdir,'rural_posterior.jpg'))

density_random <- density(d_random$`yhat[2]`)
density_weighted_naive <- density(d_weighted_naive$`yhat[2]`)
density_weighted <- density(d_weighted$`yhat[2]`)
density_all <- density(d_all$`yhat[2]`)

xlim <- c(0, 1000)
ylim <- c(0, max(c(density_random$y, density_weighted$y, density_weighted_naive$y, density_all$y)))

plot(NA, xlim=xlim, ylim=ylim, main=paste('Rural','Estimates'), xlab='Population Density', ylab='Probability')

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

# settlement areas
urban_pixels <- 48347
rural_pixels <- 28954
pixel_area <- 0.84
urban_area <- round(urban_pixels * pixel_area)
rural_area <- round(rural_pixels * pixel_area)

# total plot: settlement type 1
jpeg('out/drc/total_urban.jpg')

random_total_urban <- sum(rlnorm(urban_area, log(d_random$`med[1]`), d_random$`LOG_SIGMA[1]`))
weighted_naive_total_urban <- sum(rlnorm(urban_area, log(d_weighted_naive$`med[1]`), d_weighted_naive$`LOG_SIGMA[1]`))
weighted_total_urban <- sum(rlnorm(urban_area, log(d_weighted$`med[1]`), d_weighted$`LOG_SIGMA[1]`))
all_total_urban <- sum(rlnorm(urban_area, log(d_all$`med[1]`), d_all$`LOG_SIGMA[1]`))

bardata <- c(random_total_urban, weighted_naive_total_urban, weighted_total_urban, all_total_urban)

barplot(height=matrix(bardata, ncol=1, byrow=T), 
        beside=T, space=0.1,
        legend.text=c('Random','Weighted Unadjusted','Weighted Adjusted','All'),
        main='Urban Population Total')

dev.off()

# total plot: settlement type 2
jpeg('out/drc/total_rural.jpg')

random_total_rural <- sum(rlnorm(rural_area, log(d_random$`med[2]`), d_random$`LOG_SIGMA[2]`))
weighted_naive_total_rural <- sum(rlnorm(rural_area, log(d_weighted_naive$`med[2]`), d_weighted_naive$`LOG_SIGMA[2]`))
weighted_total_rural <- sum(rlnorm(rural_area, log(d_weighted$`med[2]`), d_weighted$`LOG_SIGMA[2]`))
all_total_rural <- sum(rlnorm(rural_area, log(d_all$`med[2]`), d_all$`LOG_SIGMA[2]`))

bardata <- c(random_total_rural, weighted_naive_total_rural, weighted_total_rural, all_total_rural)

barplot(height=matrix(bardata, ncol=1, byrow=T), 
        beside=T, space=0.1,
        legend.text=c('Random','Weighted Unadjusted','Weighted Adjusted','All'),
        main='Rural Population Total')

dev.off()

# total plot: both settlement types
jpeg('out/drc/total.jpg')

random_total <- random_total_urban + random_total_rural
weighted_naive_total <- weighted_naive_total_urban + weighted_naive_total_rural
weighted_total <- weighted_total_urban + weighted_total_rural
all_total <- all_total_urban + all_total_rural

bardata <- c(random_total, weighted_naive_total, weighted_total, all_total)

barplot(height=matrix(bardata, ncol=1, byrow=T), 
        beside=T, space=0.1,
        legend.text=c('Random','Weighted Unadjusted','Weighted Adjusted','All'),
        main='Population Total')

# abline(h=7843407.863) # Boo estimate

dev.off()

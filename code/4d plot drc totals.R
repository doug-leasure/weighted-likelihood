# cleanup
rm(list=ls())
gc()
cat("\014") 
try(dev.off())

set.seed(42)

# working directory
setwd('C:/RESEARCH/2018 GRID3 WorldPop/git/wpgp/weighted-likelihood')

# load data
d_random <- read.csv('out/drc/d_random.csv', check.names=F)
d_weighted <- read.csv('out/drc/d_weighted.csv', check.names=F)
d_weighted_naive <- read.csv('out/drc/d_weighted_naive.csv', check.names=F)
d_all <- read.csv('out/drc/d_all.csv', check.names=F)

# settlement areas
urban_pixels <- 48347
rural_pixels <- 28954
pixel_area <- 0.84
urban_area <- round(urban_pixels * pixel_area)
rural_area <- round(rural_pixels * pixel_area)

# urban totals
random_total_urban <- sum(rlnorm(urban_area, log(d_random$`med[1]`), d_random$`LOG_SIGMA[1]`))
weighted_naive_total_urban <- sum(rlnorm(urban_area, log(d_weighted_naive$`med[1]`), d_weighted_naive$`LOG_SIGMA[1]`))
weighted_total_urban <- sum(rlnorm(urban_area, log(d_weighted$`med[1]`), d_weighted$`LOG_SIGMA[1]`))
all_total_urban <- sum(rlnorm(urban_area, log(d_all$`med[1]`), d_all$`LOG_SIGMA[1]`))

# rural totals
random_total_rural <- sum(rlnorm(rural_area, log(d_random$`med[2]`), d_random$`LOG_SIGMA[2]`))
weighted_naive_total_rural <- sum(rlnorm(rural_area, log(d_weighted_naive$`med[2]`), d_weighted_naive$`LOG_SIGMA[2]`))
weighted_total_rural <- sum(rlnorm(rural_area, log(d_weighted$`med[2]`), d_weighted$`LOG_SIGMA[2]`))
all_total_rural <- sum(rlnorm(rural_area, log(d_all$`med[2]`), d_all$`LOG_SIGMA[2]`))

# totals bar plot
jpeg('manuscript/drc_totals.jpg', res=300, height=6, width=6, units='in')

bardata <- data.frame(Urban=c(random_total_urban, weighted_naive_total_urban, weighted_total_urban, all_total_urban),
                      Rural=c(random_total_rural, weighted_naive_total_rural, weighted_total_rural, all_total_rural),
                      row.names=c('Random','Weighted Unadjusted','Weighted Adjusted','Combined'))
bardata <- as.matrix(bardata)

barplot(height=bardata, 
        beside=T, 
        space=c(0.1, 0.5),
        legend.text=T,
        main='Kinshasa Population Totals',
        ylab='Population Total',
        args.legend=list(x='topright', bty='n'))

dev.off()


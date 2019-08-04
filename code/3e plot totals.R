# image file
jpeg('out/zmb_totals.jpg', res=300, height=6, width=6, units='in')

# plot function
source('code/functions/plotTotals.R')

# load data
d_random <- read.csv('out/zmb/random/d.csv', check.names=F)
d_weighted <- read.csv('out/zmb/weighted/d.csv', check.names=F)
d_weighted_naive <- read.csv('out/zmb/weighted_naive/d.csv', check.names=F)
d_combined <- read.csv('out/zmb/combined/d.csv', check.names=F)

# prepare output data
totals <- upper <- lower <- data.frame(Urban=rep(NA, 4),
                                       Rural=rep(NA, 4),
                                       row.names=c('Random','Weighted Unadjusted','Weighted Adjusted','Combined')
)

# totals
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
plotTotals(main='Zambia Six Regions Population Totals',
           bardata = as.matrix(totals), 
           lower = lower, 
           upper = upper
)
dev.off()


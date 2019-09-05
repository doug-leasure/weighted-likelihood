# image file
jpeg('out/zmb_totals_with_areas.jpg', res=300, height=6, width=6, units='in')

# plot function
source('code/functions/plotTotals.R')

# load data
d_random <- read.csv('out/zmb_with_areas/random/d.csv', check.names=F)
d_weighted <- read.csv('out/zmb_with_areas/weighted/d.csv', check.names=F)
d_weighted_naive <- read.csv('out/zmb_with_areas/weighted_naive/d.csv', check.names=F)
d_combined <- read.csv('out/zmb_with_areas/combined/d.csv', check.names=F)
clusters <- read.csv("in/zmb_smglsurvey_clusters.csv")


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

real <- matrix(rep(c(sum(clusters$N[clusters$REGION == 'Urban']),sum(clusters$N[clusters$REGION == 'Rural'])),each=4),byrow=F,ncol=2)
#real <- matrix(rep(c(sum(clusters$pop_dens[clusters$REGION == 'Urban']),sum(clusters$pop_dens[clusters$REGION == 'Rural'])),each=4),byrow=F,ncol=2)

  
rownames(real) <- c('Random','Weighted Unadjusted','Weighted Adjusted','Combined')
colnames(real) <- c('Urban','Rural')
  
# plot
plotTotals(main='Zambia Six Regions Population Totals',
           bardata = as.matrix(totals), 
           lower = lower, 
           upper = upper,
           real = real,
           pos.legend='topleft'
)
dev.off()


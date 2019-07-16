# cleanup
rm(list=ls())
gc()
cat("\014") 
try(dev.off())

set.seed(42)

# working directory
setwd('C:/RESEARCH/2018 GRID3 WorldPop/git/wpgp/weighted-likelihood')

# plot function
plotSimModel <- function(jd, d, real, real1, real2, 
                         mar=c(4.5,2.5,1,1), legend=F,
                         xlab=NA, xaxt='s',
                         main=NA, line.main=-1.5, adj.main=0.2){
  par(mar=mar)
  
  # densities
  density_y1 <- density(jd$y[jd$type==1])
  density_y2 <- density(jd$y[jd$type==2])
  
  density_real1 <- density(real1)
  density_real2 <- density(real2)
  
  density_yhat1 <- density(d$`yhat[1]`)
  density_yhat2 <- density(d$`yhat[2]`)
  
  #gray level
  g1 <- 0.6
  g2 <- 0.4
  
  # plot
  xlim <- c(0, quantile(real, probs=0.95))
  ylim <- c(0, max(density_y1$y, density_y2$y, density_real1$y, density_real2$y, density_yhat1$y, density_yhat2$y))
  
  plot(NA, xlim=xlim, ylim=ylim, xlab=xlab, ylab=NA, yaxt='n', xaxt=xaxt)
  
  polygon(y=c(density_real1$y, rep(0,length(density_real1$y))),
          x=c(density_real1$x, rev(density_real1$x)) , col=gray(g1, alpha=0.5), border=NA)
  
  polygon(y=c(density_real2$y, rep(0,length(density_real2$y))),
          x=c(density_real2$x, rev(density_real2$x)) , col=gray(g1, alpha=0.5), border=NA)
  
  
  lines(density_yhat1, col='black', lwd=2, lty=1)
  lines(density_yhat2, col=gray(g2), lwd=2, lty=1)
  
  lines(density_y1, col='black', lwd=2, lty=3)
  lines(density_y2, col=gray(g2), lwd=2, lty=3)
  
  if(legend){
    legend('topright', 
           legend=c('True','Data Urban','Model Urban','Data Rural','Model Rural'),
           col=c(NA,'black','black',gray(g2),gray(g2)),
           fill=c(gray(0.6),NA,NA,NA,NA),
           border=c(NA,NA,NA,NA,NA),
           lwd=c(NA,2,2,2,2),
           lty=c(NA,3,1,3,1),
           x.intersp=c(1,2,2,2,2),
           bty='n')
  }
  
  if(!is.na(main)){
    mtext(main, cex=1, side=3, line=line.main, adj=adj.main)
  }
  
}


# image file
jpeg('manuscript/sim_model.jpg', res=300, height=6, width=6, units='in')

# panel layout
layout(matrix(1:4, nrow=2, ncol=2, byrow=F), widths=c(1,1), heights=c(0.9,1.05))

# panel 1: random
plotSimModel(jd=readRDS('out/sims/2_random_2types/jd.rds'),
             d=read.csv('out/sims/2_random_2types/d.csv', check.names=F),
             real=readRDS('out/sims/2_random_2types/real.rds'),
             real1=readRDS('out/sims/2_random_2types/real1.rds'),
             real2=readRDS('out/sims/2_random_2types/real2.rds'),
             mar=c(0.5, 1, 1, 0.5),
             main='Random',
             xaxt='n',
             legend=T
             )

# panel 2: combined
plotSimModel(jd=readRDS('out/sims/8_combined_2types/jd.rds'),
             d=read.csv('out/sims/8_combined_2types/d.csv', check.names=F),
             real=readRDS('out/sims/8_combined_2types/real.rds'),
             real1=readRDS('out/sims/8_combined_2types/real1.rds'),
             real2=readRDS('out/sims/8_combined_2types/real2.rds'),
             mar=c(4.5, 1, 0.5, 0.5),
             xlab='Population Density',
             main='Combined'
             )


# Weighted unadjusted
plotSimModel(jd=readRDS('out/sims/4_weighted_2types_equalweights/jd.rds'),
             d=read.csv('out/sims/4_weighted_2types_equalweights/d.csv', check.names=F),
             real=readRDS('out/sims/4_weighted_2types_equalweights/real.rds'),
             real1=readRDS('out/sims/4_weighted_2types_equalweights/real1.rds'),
             real2=readRDS('out/sims/4_weighted_2types_equalweights/real2.rds'),
             mar=c(0.5, 0.5, 1, 1),
             xlab='Population Density',
             main='Weighted\nUnadjusted',
             line.main=-2.5,
             adj.main=0.25,
             xaxt='n'
)


# Weighted adjusted
plotSimModel(jd=readRDS('out/sims/8_combined_2types/jd.rds'),
             d=read.csv('out/sims/8_combined_2types/d.csv', check.names=F),
             real=readRDS('out/sims/8_combined_2types/real.rds'),
             real1=readRDS('out/sims/8_combined_2types/real1.rds'),
             real2=readRDS('out/sims/8_combined_2types/real2.rds'),
             mar=c(4.5, 0.5, 0.5, 1),
             xlab='Population Density',
             main='Weighted\nAdjusted',
             line.main=-2.5
)


dev.off()



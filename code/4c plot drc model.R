# cleanup
rm(list=ls())
gc()
cat("\014") 
try(dev.off())

set.seed(42)

# working directory
setwd('C:/RESEARCH/2018 GRID3 WorldPop/git/wpgp/weighted-likelihood')

# plot function
plotDrcModel <- function(jd, d,  
                         mar=c(4.5,2.5,1,1), legend=F,
                         xlab=NA, xaxt='s',
                         main=NA, line.main=-1.5, adj.main=0.3){
  par(mar=mar)
  
  # densities
  density_y1 <- density(jd$y[jd$type==1])
  density_y2 <- density(jd$y[jd$type==2])
  
  density_yhat1 <- density(d$`yhat[1]`)
  density_yhat2 <- density(d$`yhat[2]`)
  
  #gray level
  g2 <- 0.6
  
  # plot
  xlim <- c(0, 1000)
  ylim <- c(0, max(density_y1$y, density_y2$y, density_yhat1$y, density_yhat2$y))
  
  plot(NA, xlim=xlim, ylim=ylim, xlab=xlab, ylab=NA, yaxt='n', xaxt=xaxt)
  
  lines(density_yhat1, col='black', lwd=2, lty=1)
  lines(density_y1, col='black', lwd=2, lty=3)
  
  lines(density_yhat2, col=gray(g2), lwd=2, lty=1)
  lines(density_y2, col=gray(g2), lwd=2, lty=3)
  
  if(legend){
    legend('topright', 
           legend=c('Data Urban','Model Urban','Data Rural','Model Rural'),
           col=c('black','black',gray(g2),gray(g2)),
           border=c(NA,NA,NA,NA),
           lwd=c(2,2,2,2),
           lty=c(3,1,3,1),
           bty='n')
  }
  
  if(!is.na(main)){
    mtext(main, cex=1, side=3, line=line.main, adj=adj.main)
  }
  
}


# image file
jpeg('manuscript/drc_model.jpg', res=300, height=6, width=6, units='in')

# panel layout
layout(matrix(1:4, nrow=2, ncol=2, byrow=F), widths=c(1,1), heights=c(0.9,1.05))

# panel 1: random
plotDrcModel(jd=readRDS('out/drc/jd_random.rds'),
             d=read.csv('out/drc/d_random.csv', check.names=F),
             mar=c(0.5, 1, 1, 0.5),
             main='Random',
             xaxt='n',
             legend=T
)

# panel 2: combined
plotDrcModel(jd=readRDS('out/drc/jd_all.rds'),
             d=read.csv('out/drc/d_all.csv', check.names=F),
             mar=c(4.5, 1, 0.5, 0.5),
             xlab='Population Density',
             main='Combined'
)


# Weighted unadjusted
plotDrcModel(jd=readRDS('out/drc/jd_weighted_naive.rds'),
             d=read.csv('out/drc/d_weighted_naive.csv', check.names=F),
             mar=c(0.5, 0.5, 1, 1),
             xlab='Population Density',
             main='Weighted\nUnadjusted',
             line.main=-2.5,
             adj.main=0.25,
             xaxt='n'
)


# Weighted adjusted
plotDrcModel(jd=readRDS('out/drc/jd_weighted.rds'),
             d=read.csv('out/drc/d_weighted.csv', check.names=F),
             mar=c(4.5, 0.5, 0.5, 1),
             xlab='Population Density',
             main='Weighted\nAdjusted',
             line.main=-2.5
)


dev.off()



plotModel <- function(jd, d, real1=NA, real2=NA, 
                         mar=c(4.5,2.5,1,1), legend=F,
                         xlab=NA, xaxt='s', xmax=1e3,
                         main=NA, line.main=-1.5, adj.main=0.5){
  par(mar=mar)
  
  # densities
  density_y1 <- density(jd$y[jd$type==1], from=0, to=xmax)
  density_y2 <- density(jd$y[jd$type==2], from=0, to=xmax)
  
  if(length(real1)>1) density_real1 <- density(real1, from=0, to=xmax)
  if(length(real2)>1) density_real2 <- density(real2, from=0, to=xmax)
  
  density_yhat1 <- density(as.matrix(d[,paste0('yhat[',jd$itype1,']')]), from=0, to=xmax)
  density_yhat2 <- density(as.matrix(d[,paste0('yhat[',jd$itype2,']')]), from=0, to=xmax)
  
  #gray level
  g1 <- 0.6
  g2 <- 0.4
  
  # plot
  xlim <- c(0, xmax)
  ylim <- c(0, max(density_y1$y, density_y2$y, density_yhat1$y, density_yhat2$y))
  
  if(length(real1)>1) ylim[2] <- max(ylim[2], max(density_real1$y))
  if(length(real2)>1) ylim[2] <- max(ylim[2], max(density_real2$y))
  
  plot(NA, xlim=xlim, ylim=ylim, xlab=xlab, ylab=NA, yaxt='n', xaxt=xaxt)
  
  if(length(real1)>1) {
    polygon(y=c(density_real1$y, rep(0,length(density_real1$y))),
          x=c(density_real1$x, rev(density_real1$x)) , col=gray(g1, alpha=0.5), border=NA)
  }
  
  if(length(real2)>1) {
    polygon(y=c(density_real2$y, rep(0,length(density_real2$y))),
          x=c(density_real2$x, rev(density_real2$x)) , col=gray(g1, alpha=0.5), border=NA)
  }
  
  lines(density_yhat1, col='black', lwd=2, lty=1)
  lines(density_y1, col='black', lwd=2, lty=3)
  
  lines(density_yhat2, col=gray(g2), lwd=2, lty=1)
  lines(density_y2, col=gray(g2), lwd=2, lty=3)
  
  if(legend){
    if(length(real1)>1 | length(real2)>1){
      legend('topright', 
             legend=c('True','Data Urban','Model Urban','Data Rural','Model Rural'),
             col=c(NA,'black','black',gray(g2),gray(g2)),
             fill=c(gray(0.6),NA,NA,NA,NA),
             border=c(NA,NA,NA,NA,NA),
             lwd=c(NA,2,2,2,2),
             lty=c(NA,3,1,3,1),
             x.intersp=c(1,2,2,2,2),
             bty='n')
      
    } else {
      legend('topright', 
             legend=c('Data Urban','Model Urban','Data Rural','Model Rural'),
             col=c('black','black',gray(g2),gray(g2)),
             border=c(NA,NA,NA,NA),
             lwd=c(2,2,2,2),
             lty=c(3,1,3,1),
             bty='n')
    }
  }
  
  if(!is.na(main)){
    mtext(main, cex=1, side=3, line=line.main, adj=adj.main)
  }
  
}

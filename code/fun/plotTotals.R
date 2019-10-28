plotTotals <- function(dat, file, pos.legend = 'topright', plotReal=F, main='Population Totals'){
  # pos.legend='topright'
  # plotReal=F
  # main='Population Totals'
  
  bd <- as.matrix(dat$totals)
  lower <- dat$lower
  upper <- dat$upper
  real <- dat$real
  
  # image file
  jpeg(file, res=300, height=4, width=4, units='in')
  
  # bar plot
  par(mar=c(2.5,4.5,1,1))
  
  bp <- barplot(height=bd, 
                main=NA,
                beside=T,
                space=c(0.1, 0.5),
                ylim=c(0,max(upper)),
                ylab='Population Total',
                legend.text=T,
                args.legend=list(x=pos.legend, bty='n'))
  
  row.names(bp) <- row.names(bd)
  colnames(bp) <- colnames(bd)
  
  # add error bars and real values (if known)
  for(i in row.names(bp)){
    for(j in colnames(bp)){
      arrows(x0=bp[i,j], 
             y0=lower[i,j],
             y1=upper[i,j],
             length=0.1, angle=90, lwd=1, code=3
      )
      if(plotReal){
        arrows(x0=bp[i,j]-0.5, x1=bp[i,j]+0.5, y0=real[i,j], length=0, lty=1, lwd=2)
      }
    }
  }
  dev.off()
}
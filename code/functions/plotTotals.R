plotTotals <- function(bardata, lower, upper, pos.legend = 'topright',real=NA, main='Population Totals'){
  
  # bar plot
  bp <- barplot(height=bardata, 
                main=main,
                beside=T,
                space=c(0.1, 0.5),
                ylim=c(0,max(upper)),
                ylab='Population Total',
                legend.text=T,
                args.legend=list(x=pos.legend, bty='n'))
  
  # add error bars
  arrows(x0=bp[1,1], x1=bp[1,1],
         y0=lower['Random','Urban'],
         y1=upper['Random','Urban'],
         length=0.1, angle=90, lwd=1, code=3
  )
  arrows(x0=bp[1,2], x1=bp[1,2],
         y0=lower['Random','Rural'],
         y1=upper['Random','Rural'],
         length=0.1, angle=90, lwd=1, code=3
  )
  arrows(x0=bp[2,1], x1=bp[2,1],
         y0=lower['Weighted Unadjusted','Urban'],
         y1=upper['Weighted Unadjusted','Urban'],
         length=0.1, angle=90, lwd=1, code=3
  )
  arrows(x0=bp[2,2], x1=bp[2,2],
         y0=lower['Weighted Unadjusted','Rural'],
         y1=upper['Weighted Unadjusted','Rural'],
         length=0.1, angle=90, lwd=1, code=3
  )
  arrows(x0=bp[3,1], x1=bp[3,1],
         y0=lower['Weighted Adjusted','Urban'],
         y1=upper['Weighted Adjusted','Urban'],
         length=0.1, angle=90, lwd=1, code=3
  )
  arrows(x0=bp[3,2], x1=bp[3,2],
         y0=lower['Weighted Adjusted','Rural'],
         y1=upper['Weighted Adjusted','Rural'],
         length=0.1, angle=90, lwd=1, code=3
  )
  arrows(x0=bp[4,1], x1=bp[4,1],
         y0=lower['Combined','Urban'],
         y1=upper['Combined','Urban'],
         length=0.1, angle=90, lwd=1, code=3
  )
  arrows(x0=bp[4,2], x1=bp[4,2],
         y0=lower['Combined','Rural'],
         y1=upper['Combined','Rural'],
         length=0.1, angle=90, lwd=1, code=3
  )
  
  # add correct value
  if(!is.na(real)) {
    arrows(x0=bp[1,1]-0.5, x1=bp[1,1]+0.5, y0=real['Random','Urban'], length=0, lty=1, lwd=2)
    arrows(x0=bp[2,1]-0.5, x1=bp[2,1]+0.5, y0=real['Weighted Unadjusted','Urban'], length=0, lty=1, lwd=2)
    arrows(x0=bp[3,1]-0.5, x1=bp[3,1]+0.5, y0=real['Weighted Adjusted','Urban'], length=0, lty=1, lwd=2)
    arrows(x0=bp[4,1]-0.5, x1=bp[4,1]+0.5, y0=real['Combined','Urban'], length=0, lty=1, lwd=2)
    
    arrows(x0=bp[1,2]-0.5, x1=bp[1,2]+0.5, y0=real['Random','Rural'], length=0, lty=1, lwd=2)
    arrows(x0=bp[2,2]-0.5, x1=bp[2,2]+0.5, y0=real['Weighted Unadjusted','Rural'], length=0, lty=1, lwd=2)
    arrows(x0=bp[3,2]-0.5, x1=bp[3,2]+0.5, y0=real['Weighted Adjusted','Rural'], length=0, lty=1, lwd=2)
    arrows(x0=bp[4,2]-0.5, x1=bp[4,2]+0.5, y0=real['Combined','Rural'], length=0, lty=1, lwd=2)
  }
}
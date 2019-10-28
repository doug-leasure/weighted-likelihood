plotModelPanel <- function(file, sims, dir, plotReal=F, xmax=1e3){
  
  # image file
  jpeg(file, res=300, height=6, width=6, units='in')
  
  # panel layout
  layout(matrix(1:4, nrow=2, ncol=2, byrow=F), widths=c(1,1), heights=c(0.9,1.05))
  
  # panel 1: random
  if(plotReal){
    real1 <- readRDS(paste0(dir,sims[1],'/real1.rds'))
    real2 <- readRDS(paste0(dir,sims[1],'/real2.rds'))
  } else {
    real1 <- real2 <- NA
  }
  
  plotModel(jd=readRDS(paste0(dir,sims[1],'/jd.rds')),
            d=read.csv(paste0(dir,sims[1],'/d.csv'), check.names=F),
            real1=real1,
            real2=real2,
            main=sims[1],
            mar=c(0.5, 1, 1, 0.5),
            adj.main=0.25,
            xaxt='n',
            xmax=xmax,
            legend=T
  )
  
  # panel 2: combined
  if(plotReal){
    real1 <- readRDS(paste0(dir,sims[2],'/real1.rds'))
    real2 <- readRDS(paste0(dir,sims[2],'/real2.rds'))
  } else {
    real1 <- real2 <- NA
  }
  
  plotModel(jd=readRDS(paste0(dir,sims[2],'/jd.rds')),
            d=read.csv(paste0(dir,sims[2],'/d.csv'), check.names=F),
            real1=real1,
            real2=real2,
            main=sims[2],
            mar=c(4.5, 1, 0.5, 0.5),
            xmax=xmax,
            xlab='Population Density'
  )
  
  # panel 3: weighted unadjusted
  if(plotReal){
    real1 <- readRDS(paste0(dir,sims[3],'/real1.rds'))
    real2 <- readRDS(paste0(dir,sims[3],'/real2.rds'))
  } else {
    real1 <- real2 <- NA
  }
  
  plotModel(jd=readRDS(paste0(dir,sims[3],'/jd.rds')),
            d=read.csv(paste0(dir,sims[3],'/d.csv'), check.names=F),
            real1=real1,
            real2=real2,
            main=sims[3],
            mar=c(0.5, 0.5, 1, 1),
            xmax=xmax,
            xlab='Population Density',
            line.main=-2.5,
            xaxt='n'
  )
  
  
  # panel 4: weighted adjusted
  if(plotReal){
    real1 <- readRDS(paste0(dir,sims[4],'/real1.rds'))
    real2 <- readRDS(paste0(dir,sims[4],'/real2.rds'))
  } else {
    real1 <- real2 <- NA
  }
  
  plotModel(jd=readRDS(paste0(dir,sims[4],'/jd.rds')),
            d=read.csv(paste0(dir,sims[4],'/d.csv'), check.names=F),
            real1=real1,
            real2=real2,
            main=sims[4],
            mar=c(4.5, 0.5, 0.5, 1),
            xmax=xmax,
            xlab='Population Density',
            line.main=-2.5
  )
  dev.off()
}


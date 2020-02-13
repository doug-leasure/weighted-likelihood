#' Creates a multipanel plot using plotModel()
#'
#' @param file File name to save jpeg image of plot
#' @param sims The simulation names to be included in the plot (must match directory names for simulation results)
#' @param dir Root directory containing simulation results
#' @param plotReal Logical to include the true population sizes in the plot
#' @param xmax Maximum value for the x-axis in plots
#'
#' @return Writes the plot to disk
#'
#' @export

plotModelPanel <- function(file, sims, dir, plotReal=F, xmax=1e3){

  # image file
  jpeg(file, res=300, height=6, width=6, units='in')

  # panel layout
  layout(matrix(1:4, nrow=2, ncol=2, byrow=F), widths=c(1,1), heights=c(0.9,1.05))

  # panel 1: random
  sim1 <- readRDS(file.path(dir,sims[1],'sim1.rds'))
  sim2 <- readRDS(file.path(dir,sims[1],'sim2.rds'))

  if(plotReal){
    real1 <- sim1$D
    real2 <- sim2$D
  } else {
    real1 <- real2 <- NA
  }

  plotModel(jd=readRDS(file.path(dir,sims[1],'jd.rds')),
            d=readRDS(file.path(dir,sims[1],'d.rds')),
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
  sim1 <- readRDS(file.path(dir,sims[2],'sim1.rds'))
  sim2 <- readRDS(file.path(dir,sims[2],'sim2.rds'))

  if(plotReal){
    real1 <- sim1$D
    real2 <- sim2$D
  } else {
    real1 <- real2 <- NA
  }

  plotModel(jd=readRDS(file.path(dir,sims[2],'jd.rds')),
            d=readRDS(file.path(dir,sims[2],'d.rds')),
            real1=real1,
            real2=real2,
            main=sims[2],
            mar=c(4.5, 1, 0.5, 0.5),
            xmax=xmax,
            xlab='Population Density'
  )

  # panel 3: weighted unadjusted
  sim1 <- readRDS(file.path(dir,sims[3],'sim1.rds'))
  sim2 <- readRDS(file.path(dir,sims[3],'sim2.rds'))

  if(plotReal){
    real1 <- sim1$D
    real2 <- sim2$D
  } else {
    real1 <- real2 <- NA
  }

  plotModel(jd=readRDS(file.path(dir,sims[3],'jd.rds')),
            d=readRDS(file.path(dir,sims[3],'d.rds')),
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
  sim1 <- readRDS(file.path(dir,sims[4],'sim1.rds'))
  sim2 <- readRDS(file.path(dir,sims[4],'sim2.rds'))

  if(plotReal){
    real1 <- sim1$D
    real2 <- sim2$D
  } else {
    real1 <- real2 <- NA
  }

  plotModel(jd=readRDS(file.path(dir,sims[4],'jd.rds')),
            d=readRDS(file.path(dir,sims[4],'d.rds')),
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


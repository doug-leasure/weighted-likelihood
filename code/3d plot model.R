# image file
jpeg('out/zmb_model.jpg', res=300, height=6, width=6, units='in')

# plot function
source('code/functions/plotModel.R')

# panel layout
layout(matrix(1:4, nrow=2, ncol=2, byrow=F), widths=c(1,1), heights=c(0.9,1.05))

# panel 1: random
plotModel(jd=readRDS('out/zmb/random/jd.rds'),
          d=read.csv('out/zmb/random/d.csv', check.names=F),
          mar=c(0.5, 1, 1, 0.5),
          main='Random',
          xaxt='n',
          max.x = 300,
          legend=T,
          adj.main=0.3
)

# panel 2: combined
plotModel(jd=readRDS('out/zmb/combined/jd.rds'),
          d=read.csv('out/zmb/combined/d.csv', check.names=F),
          mar=c(4.5, 1, 0.5, 0.5),
          xlab='Population Density',
          max.x = 300,
          main='Combined'
)


# panel 3: weighted unadjusted
plotModel(jd=readRDS('out/zmb/weighted_naive/jd.rds'),
          d=read.csv('out/zmb/weighted_naive/d.csv', check.names=F),
          mar=c(0.5, 0.5, 1, 1),
          xlab='Population Density',
          main='Weighted\nUnadjusted',
          line.main=-2.5,
          max.x = 300,
          xaxt='n'
)


# panel 4: weighted adjusted
plotModel(jd=readRDS('out/zmb/weighted/jd.rds'),
          d=read.csv('out/zmb/weighted/d.csv', check.names=F),
          mar=c(4.5, 0.5, 0.5, 1),
          xlab='Population Density',
          main='Weighted\nAdjusted',
          max.x = 300,
          line.main=-2.5
)

dev.off()



# image file
jpeg('out/sim_model.jpg', res=300, height=6, width=6, units='in')

# plot function
source('code/functions/plotModel.R')

# panel layout
layout(matrix(1:4, nrow=2, ncol=2, byrow=F), widths=c(1,1), heights=c(0.9,1.05))

# panel 1: random
plotModel(jd=readRDS('out/sims/random/jd.rds'),
             d=read.csv('out/sims/random/d.csv', check.names=F),
             real1=readRDS('out/sims/random/real1.rds'),
             real2=readRDS('out/sims/random/real2.rds'),
             mar=c(0.5, 1, 1, 0.5),
             main='Random',
             adj.main=0.25,
             xaxt='n',
             legend=T
             )

# panel 2: combined
plotModel(jd=readRDS('out/sims/combined/jd.rds'),
             d=read.csv('out/sims/combined/d.csv', check.names=F),
             real1=readRDS('out/sims/combined/real1.rds'),
             real2=readRDS('out/sims/combined/real2.rds'),
             mar=c(4.5, 1, 0.5, 0.5),
             xlab='Population Density',
             main='Combined'
             )


# panel 3: weighted unadjusted
plotModel(jd=readRDS('out/sims/weighted_naive/jd.rds'),
             d=read.csv('out/sims/weighted_naive/d.csv', check.names=F),
             real1=readRDS('out/sims/weighted_naive/real1.rds'),
             real2=readRDS('out/sims/weighted_naive/real2.rds'),
             mar=c(0.5, 0.5, 1, 1),
             xlab='Population Density',
             main='Weighted\nUnadjusted',
             line.main=-2.5,
             xaxt='n'
)


# panel 4: weighted adjusted
plotModel(jd=readRDS('out/sims/weighted/jd.rds'),
             d=read.csv('out/sims/weighted/d.csv', check.names=F),
             real1=readRDS('out/sims/weighted/real1.rds'),
             real2=readRDS('out/sims/weighted/real2.rds'),
             mar=c(4.5, 0.5, 0.5, 1),
             xlab='Population Density',
             main='Weighted\nAdjusted',
             line.main=-2.5
)


dev.off()



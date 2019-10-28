dataZMB <- function(indir='in/', outdir='out/zmb/', areaAdjust=F, seed=42){
  set.seed(seed)
  
  # read in data
  clusters <- read.csv("in/zmb_smglsurvey_clusters.csv")
  
  # total clusters = 1735
  # total rural (type 2) clusters = 1545
  # total urban (type 1) clusters = 190
  
  n.rural.weighted <- n.rural.random <- 500
  n.urban.weighted <- n.urban.random <- 100 
  
  # sample weights
  r.clusters <- clusters[clusters$REGION == "Rural",]
  u.clusters <- clusters[clusters$REGION == "Urban",]
  
  r.clust <- r.clusters$pop_dens
  u.clust <- u.clusters$pop_dens
  
  weights1 <- weights_calc(numerator1=r.clust, denominator=sum(r.clust))
  weights2 <- weights_calc(numerator1=u.clust, denominator=sum(u.clust))
  weights <- c(weights1, weights2)
  
  # draw a pop-weighted sample
  weighted_clusters <- c(sample(r.clusters$CCLUST, n.rural.weighted, replace=F, prob=weights1),
                         sample(u.clusters$CCLUST, n.urban.weighted, replace=F, prob=weights2))
  
  # draw a random sample
  random_clusters <- c(sample(r.clusters$CCLUST, n.rural.random, replace=F),
                       sample(u.clusters$CCLUST, n.urban.random, replace=F))
  
  # combine weighted + random
  random_weighted_clusters <- c(weighted_clusters, random_clusters)
  
  # weights for weighted
  r.weights <- weights1[r.clusters$CCLUST %in% weighted_clusters[1:n.rural.weighted]]
  u.weights <- weights2[u.clusters$CCLUST %in% weighted_clusters[(n.rural.weighted+1):length(weighted_clusters)]]
  
  inv.w.weighted <- weights_calc(c(r.weights,u.weights),length(c(r.weights,u.weights)),inverse=T)
  
  # weights for random
  w.random <- c(rep(1/nrow(r.clusters),n.rural.random), rep(1/nrow(u.clusters),n.urban.random))
  
  inv.w.random <- weights_calc(w.random,sum(w.random))
  
  # weights for combined
  w.combined <- weights_calc(c(r.weights,u.weights,w.random),length(c(r.weights,u.weights,w.random)),inverse=T)
  ######## not sure if this is the same as doing the numerator2, I know this way will work ######
  
  NApad <- length(r.clusters$sett_area) - length(u.clusters$sett_area)
  
  if(areaAdjust){
    areas <- cbind(c(u.clusters$sett_area, rep(NA, NApad)),r.clusters$sett_area)
  } else {
    areas <- 1
  }
  
  # jags data random
  jd_random <- list(id = random_clusters,
                    
                    y = c(r.clusters$pop_dens[r.clusters$CCLUST %in% random_clusters[1:n.rural.random]], u.clusters$pop_dens[u.clusters$CCLUST %in% random_clusters[(n.rural.random+1):length(random_clusters)]]),
                    w = inv.w.random,
                    type = c(rep(2,n.rural.random), rep(1,n.urban.random)),
                    
                    n = n.rural.random + n.urban.random,
                    ntype = 2,
                    itype2 = 1:n.rural.random,
                    itype1 = (n.rural.random+1):(n.rural.random + n.urban.random),
                    
                    a = areas,
                    ntotal = c(nrow(u.clusters),nrow(r.clusters)),
                    seed = seed
  )
  
  # jags data weighted
  jd_weighted <- list(id = weighted_clusters,
                      
                      y = c(r.clusters$pop_dens[r.clusters$CCLUST %in% weighted_clusters[1:n.rural.weighted]], u.clusters$pop_dens[u.clusters$CCLUST %in% weighted_clusters[(n.rural.weighted+1):length(weighted_clusters)]]),
                      w = inv.w.weighted,
                      type = c(rep(2,n.rural.weighted), rep(1,n.urban.weighted)),
                      
                      n = n.rural.weighted + n.urban.weighted,
                      ntype = 2,
                      itype2 = 1:n.rural.weighted,
                      itype1 = (n.rural.weighted+1):(n.rural.weighted + n.urban.weighted),
                      
                      a = areas,
                      ntotal = c(nrow(u.clusters),nrow(r.clusters)),
                      seed = seed
  )
  
  # jags data weighted without model weights
  jd_weighted_naive <- list(id = weighted_clusters,
                            
                            y = c(r.clusters$pop_dens[r.clusters$CCLUST %in% weighted_clusters[1:n.rural.weighted]], u.clusters$pop_dens[u.clusters$CCLUST %in% weighted_clusters[(n.rural.weighted+1):length(weighted_clusters)]]),
                            w = rep(1/(n.rural.weighted + n.urban.weighted), n.rural.weighted + n.urban.weighted),
                            type = c(rep(2,n.rural.weighted), rep(1,n.urban.weighted)),
                            
                            n = n.rural.weighted + n.urban.weighted,
                            ntype = 2,
                            itype2 = 1:n.rural.weighted,
                            itype1 = (n.rural.weighted+1):(n.rural.weighted + n.urban.weighted),
                            
                            a = areas,
                            ntotal = c(nrow(u.clusters),nrow(r.clusters)),
                            seed = seed 
  )
  
  # jags data weighted and random
  jd_all <- list(id = c(weighted_clusters, random_clusters),
                 
                 y = c(r.clusters$pop_dens[r.clusters$CCLUST %in% weighted_clusters[1:n.rural.weighted]], u.clusters$pop_dens[u.clusters$CCLUST %in% weighted_clusters[(n.rural.weighted+1):length(weighted_clusters)]],
                       r.clusters$pop_dens[r.clusters$CCLUST %in% random_clusters[1:n.rural.random]], u.clusters$pop_dens[u.clusters$CCLUST %in% random_clusters[(n.rural.random+1):length(random_clusters)]]),
                 w = w.combined,
                 type = c(rep(2,n.rural.weighted), rep(1,n.urban.weighted), rep(2,n.rural.random), rep(1,n.urban.random)),
                 
                 n = n.rural.weighted + n.urban.weighted + n.rural.random + n.urban.random,
                 ntype = 2,
                 itype2 = c(1:n.rural.weighted, (n.rural.weighted + n.urban.weighted + 1):(n.rural.weighted + n.urban.weighted + n.rural.random)),
                 itype1 = c((n.rural.weighted+1):(n.rural.weighted + n.urban.weighted) , (n.rural.weighted + n.urban.weighted + n.rural.random + 1):(n.rural.weighted + n.urban.weighted + n.rural.random + n.urban.random)),
                 
                 a = areas,
                 ntotal = c(nrow(u.clusters),nrow(r.clusters)),
                 seed = seed
  )
  
  # write jags data to disk
  saveRDS(jd_random, file=paste0(outdir,'random/jd.rds'))
  saveRDS(jd_weighted, file=paste0(outdir,'weighted/jd.rds'))
  saveRDS(jd_weighted_naive, file=paste0(outdir,'weighted_naive/jd.rds'))
  saveRDS(jd_all, file=paste0(outdir,'combined/jd.rds'))  
}

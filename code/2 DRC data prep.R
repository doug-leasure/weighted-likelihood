# cleanup
rm(list=ls())
gc()
cat("\014") 
try(dev.off())

# load packages
library(dplyr); library(sf); library(reshape2); library(tidyr); library(purrr); library(svDialogs)

# working directory
setwd('C:/RESEARCH/2018 GRID3 WorldPop/git/wpgp/weighted-likelihood')

# create sub-directories
dir.create('out', showWarnings=F)
dir.create('out/drc', showWarnings=F)

# load data
clusters <- read.csv("in/cod_survey_clusters.csv")
weights <- st_read('in/gis/samlingWeights_clusters.shp')
cod_survey_covariates <- read.csv("in/cod_survey_covariate.csv", row.names = 1)

#  filter clusters with no residents 
clusters$ucla_cluster_id <- tolower(clusters$ucla_cluster_id)
clusters <- clusters %>% filter(ucla_cluster_resident_count !=0 & !is.na(ucla_cluster_resident_count) )

clusters <- clusters %>% mutate(ucla_cluster_id=as.character(ucla_cluster_id)) %>% arrange(ucla_cluster_id)
row.names(clusters) <- clusters$ucla_cluster_id

# settlement type
cod_survey_covariates$ucla_cluster_id <- tolower(cod_survey_covariates$ucla_cluster_id)

ornl_settlementType <- cod_survey_covariates %>% dplyr::select(ucla_cluster_id, ornl_settlement_bua_2016_density_mean, 
                                                  ornl_settlement_ham_2016_density_mean, ornl_settlement_ssa_2016_density_mean)
for (i in 1:nrow(ornl_settlementType)) {
  ornl_settlementType$ornl_settlement_type_2016[i] <- ifelse(ornl_settlementType$ornl_settlement_bua_2016_density[i] >
                                                               ornl_settlementType$ornl_settlement_ham_2016_density[i] +
                                                               ornl_settlementType$ornl_settlement_ssa_2016_density[i], 1, 2)
}

ornl_settlementType <- ornl_settlementType %>% dplyr::select(ucla_cluster_id, ornl_settlement_type_2016)

clusters <- clusters %>% left_join(ornl_settlementType)
rm(ornl_settlementType)

# numeric cluster id
clusters$ucla_cluster_num <-  1:length(unique(clusters$ucla_cluster_id))

# population densities
clusters <- clusters %>% left_join(cod_survey_covariates %>% dplyr::select(ucla_cluster_id,ornl_settlementCluster_2016_density_sum )) %>% 
            mutate(pop_density = ucla_cluster_resident_count/ ornl_settlementCluster_2016_density_sum)

clusters$area <- clusters$ornl_settlementCluster_2016_density_sum

# master dataframe
clusters %>% group_by(ucla_cluster_id) %>% 
  summarize(n_distinct(ucla_cluster_id), n_distinct(bcr_admin3_id), n_distinct(bcr_admin1_id)) %>% 
  unnest() %>% 
  dplyr::select(-ucla_cluster_id) %>% 
  colSums()

master_df <- clusters %>% dplyr::select(ucla_cluster_id, ucla_cluster_num, ucla_admin1_name, ucla_survey_year,
                                        ucla_cluster_resident_count, pop_density, area,
                                        ornl_settlement_type_2016, ucla_cluster_longitude, ucla_cluster_latitude ) 
# kinshasa only
master_df <- master_df[master_df$ucla_admin1_name=='Kinshasa',]

# model weights
weights$ucla_cluster_id <- tolower(weights$mez_id)
weights$weight <- weights$str_pop/weights$psu_pop

master_df <- master_df %>% left_join(weights %>% dplyr::select(ucla_cluster_id, weight))

random_weights <- sum(weights$weight)/master_df %>% ungroup() %>% 
  filter(ucla_survey_year==2017) %>% summarize(n()) %>% unlist()
master_df <- master_df %>% mutate(weight=ifelse(ucla_survey_year==2017,random_weights, weight )) 
master_df <- master_df %>%  mutate(weight_scaled=weight/sum(master_df$weight))

# jags data random
i.random <- which(master_df$ucla_survey_year==2017)
jd_random <- list(id = master_df$ucla_cluster_num[i.random],
                  
                  y = master_df$pop_density[i.random],
                  w = master_df$weight_scaled[i.random] / sum(master_df$weight_scaled[i.random]),
                  type = master_df$ornl_settlement_type_2016[i.random],
                  
                  ni = length(i.random),
                  ntype = length(unique(master_df$ornl_settlement_type_2016[i.random])),
                  itype1 = which(master_df$ornl_settlement_type_2016[i.random]==1),
                  itype2 = which(master_df$ornl_settlement_type_2016[i.random]==2),
                  itype3 = which(master_df$ornl_settlement_type_2016[i.random]==3) 
                  )

# jags data weighted
i.weighted <- which(master_df$ucla_survey_year==2018)
jd_weighted <- list(id = master_df$ucla_cluster_num[i.weighted],
                    
                    y = master_df$pop_density[i.weighted],
                    w= master_df$weight_scaled[i.weighted],
                    type = master_df$ornl_settlement_type_2016[i.weighted],
                    
                    ni = length(i.weighted),
                    ntype = length(unique(master_df$ornl_settlement_type_2016[i.weighted])),
                    itype1 = which(master_df$ornl_settlement_type_2016[i.weighted]==1),
                    itype2 = which(master_df$ornl_settlement_type_2016[i.weighted]==2),
                    itype3 = which(master_df$ornl_settlement_type_2016[i.weighted]==3) 
                    )

# jags data random and weighted
jd_all <- list(id = master_df$ucla_cluster_num,
               
               y = master_df$pop_density,
               w= master_df$weight_scaled,
               type = master_df$ornl_settlement_type_2016,
               
               ni = nrow(master_df),
               ntype = length(unique(master_df$ornl_settlement_type_2016)),
               itype1 = which(master_df$ornl_settlement_type_2016==1),
               itype2 = which(master_df$ornl_settlement_type_2016==2),
               itype3 = which(master_df$ornl_settlement_type_2016==3) 
               )

# write jags data to disk
saveRDS(jd_random, file=paste0('out/drc/jd_random.rds'))
saveRDS(jd_weighted, file=paste0('out/drc/jd_weighted.rds'))
saveRDS(jd_all, file=paste0('out/drc/jd_all.rds'))

# density plot
jpeg('out/drc/data.jpg')

density_random <- density(jd_random$y)
density_random_urb <- density(jd_random$y[jd_random$type==1])
density_random_rur <- density(jd_random$y[jd_random$type==2])

density_weighted <- density(jd_weighted$y)
density_weighted_urb <- density(jd_weighted$y[jd_weighted$type==1])
density_weighted_rur <- density(jd_weighted$y[jd_weighted$type==2])

xlim <- c(0, quantile(c(density_random$x, density_random_urb$x, density_random_rur$x, 
                        density_weighted$x, density_weighted_urb$x, density_weighted_rur$x), probs=0.99))
ylim <- c(0, max(density_random$y, density_random_urb$y, density_random_rur$y, 
                 density_weighted$y, density_weighted_urb$y, density_weighted_rur$y))

plot(NA, xlim=xlim, ylim=ylim, xlab='Population Density', ylab='Probability')

lines(density_random, col='black', lwd=2, lty=2)
lines(density_random_urb, col='red', lwd=1, lty=2)
lines(density_random_rur, col='darkgreen', lwd=1, lty=2)

lines(density_weighted, col='black', lwd=2, lty=1)
lines(density_weighted_urb, col='red', lwd=1, lty=1)
lines(density_weighted_rur, col='darkgreen', lwd=1, lty=1)

legend('topright',legend=c('random','random urban','random rural','weighted','weighted urban','weighted rural'),
       lty=c(2,2,2,1,1,1), lwd=c(2,1,1,2,1,1), col=c('black','red','darkgreen','black','red','darkgreen'))

dev.off()

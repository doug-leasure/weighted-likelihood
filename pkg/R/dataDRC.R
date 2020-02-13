#' Creates jags data for the DRC analysis
#'
#' @param indir Directory with input data
#' @param outdir Directory where to store output data
#' @param seed Random seed
#'
#' @return JAGS data objects are saved to disk
#'
#' @export

dataDRC <- function(indir='in', outdir='out/drc', seed=42){

  set.seed(seed)

  # load data
  clusters <- read.csv(file.path(indir,"cod_survey_clusters.csv"))
  weights <- st_read(file.path(indir,'gis/samlingWeights_clusters.shp'))
  cod_survey_covariates <- read.csv(file.path(indir,"cod_survey_covariate.csv"), row.names = 1)

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
  pixelarea <- 0.84
  clusters <- clusters %>% left_join(cod_survey_covariates %>% dplyr::select(ucla_cluster_id,ornl_settlementCluster_2016_density_sum )) %>%
    mutate(pop_density = ucla_cluster_resident_count / (ornl_settlementCluster_2016_density_sum * pixelarea))

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

  # remove outliers in weights (n=2)
  master_df <- master_df[master_df$weight_scaled < 0.02, ]

  # # remove outliers in density (n=2; likely errors in settled area)
  # master_df <- master_df[master_df$pop_density < 1000,]

  # settlement areas
  urban_pixels <- round(48347 / 10)
  rural_pixels <- round(28954 / 10)
  pixel_area <- 0.84
  urban_area <- round(urban_pixels * pixel_area)
  rural_area <- round(rural_pixels * pixel_area)

  # jags data random
  i.random <- which(master_df$ucla_survey_year==2017)
  jd_random <- list(id = master_df$ucla_cluster_num[i.random],

                    y = master_df$pop_density[i.random],
                    w = master_df$weight_scaled[i.random] / sum(master_df$weight_scaled[i.random]),
                    type = master_df$ornl_settlement_type_2016[i.random],

                    n = length(i.random),
                    ntype = length(unique(master_df$ornl_settlement_type_2016[i.random])),
                    itype1 = which(master_df$ornl_settlement_type_2016[i.random]==1),
                    itype2 = which(master_df$ornl_settlement_type_2016[i.random]==2),

                    a = pixel_area,
                    ntotal = c(urban_pixels, rural_pixels),
                    seed = seed
  )

  # jags data weighted
  i.weighted <- which(master_df$ucla_survey_year==2018)

  jd_weighted <- list(id = master_df$ucla_cluster_num[i.weighted],

                      y = master_df$pop_density[i.weighted],
                      w = master_df$weight_scaled[i.weighted] / sum(master_df$weight_scaled[i.weighted]),
                      type = master_df$ornl_settlement_type_2016[i.weighted],

                      n = length(i.weighted),
                      ntype = length(unique(master_df$ornl_settlement_type_2016[i.weighted])),
                      itype1 = which(master_df$ornl_settlement_type_2016[i.weighted]==1),
                      itype2 = which(master_df$ornl_settlement_type_2016[i.weighted]==2),

                      a = pixel_area,
                      ntotal = c(urban_pixels, rural_pixels),
                      seed = seed
  )

  # jags data weighted without model weights
  jd_weighted_naive <- list(id = master_df$ucla_cluster_num[i.weighted],

                            y = master_df$pop_density[i.weighted],
                            w = rep(1/length(i.weighted), length(i.weighted)),
                            type = master_df$ornl_settlement_type_2016[i.weighted],

                            n = length(i.weighted),
                            ntype = length(unique(master_df$ornl_settlement_type_2016[i.weighted])),
                            itype1 = which(master_df$ornl_settlement_type_2016[i.weighted]==1),
                            itype2 = which(master_df$ornl_settlement_type_2016[i.weighted]==2),

                            a = pixel_area,
                            ntotal = c(urban_pixels, rural_pixels),
                            seed = seed
  )

  # jags data random and weighted

  jd_all <- list(id = master_df$ucla_cluster_num,

                 y = master_df$pop_density,
                 w = master_df$weight_scaled / sum(master_df$weight_scaled),
                 type = master_df$ornl_settlement_type_2016,

                 n = nrow(master_df),
                 ntype = length(unique(master_df$ornl_settlement_type_2016)),
                 itype1 = which(master_df$ornl_settlement_type_2016==1),
                 itype2 = which(master_df$ornl_settlement_type_2016==2),

                 a = pixel_area,
                 ntotal = c(urban_pixels, rural_pixels),
                 seed = seed
  )

  # write jags data to disk
  saveRDS(jd_random, file=file.path(outdir,'random/jd.rds'))
  saveRDS(jd_weighted, file=file.path(outdir,'weighted/jd.rds'))
  saveRDS(jd_weighted_naive, file=file.path(outdir,'weighted_naive/jd.rds'))
  saveRDS(jd_all, file=file.path(outdir,'combined/jd.rds'))

}


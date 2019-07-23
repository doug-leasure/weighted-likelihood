# cleanup
rm(list=ls())
gc()
cat("\014") 
try(dev.off())

set.seed(42)

# load libraries
library(raster); library(rgdal); library(rgeos); library(tmap); library(tcltk2)

# working directory
setwd('C:/RESEARCH/2018 GRID3 WorldPop/git/wpgp/weighted-likelihood')

# load data
boundaries_kinshasa <- readOGR("in/gis", "Quartier_Kinshasa")
sampled <- readOGR("in/gis", "cod_microcensusCluster_20172018_centroid")
strata <- raster("in/gis/strata.tif")

# process data
urban <- c("Maluku", "Mont Ngafula", "Nsele")
boundaries_kinshasa$urban <- grepl(paste(urban,collapse="|"), boundaries_kinshasa$Commune)

strata <- reclassify(strata, cbind(c(1,2,3), c(1,2,2)))

strata_kinshasa <- mask(strata, boundaries_kinshasa)

strata_kinshasa <- crop(strata_kinshasa, boundaries_kinshasa)

sampled_kinshasa <- sampled[boundaries_kinshasa,]

rm(urban, sampled, strata)

# mapping

# tmap_mode("view")

sampling_kinshasa <-  

  # faceting
  tm_shape(boundaries_kinshasa) + 
  tm_fill(col="#f0f0f0") +
  tm_facets(by="urban", drop.units=F) +

  # background 
  tm_shape(boundaries_kinshasa) +  
  tm_fill(col="#f0f0f0") +

  # strata  
  tm_shape(strata_kinshasa) +
  tm_raster(palette = c("#969696", "#525252"), style="cat", title="Strata", labels=c("[1] Urban", "[2] Rural")) +

  # samples  
  tm_shape(sampled_kinshasa) +
  tm_symbols(size=.2, shape="Year", alpha=0, border.col=c("black")) +
  tmap_options(max.raster = c(plot = 13247882, view = 13247882)) +

  # layout
  tm_layout(panel.show=F, legend.show=F, legend.outside=F, legend.position=c("left", "bottom"))

# saving
tmap_save(sampling_kinshasa, width=3.4, height=4.3, units="in", filename = "out/kinshasa_sampling.jpg")

          
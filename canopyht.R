library(sf)
library(terra)
library(ranger)
library(rpart)
library(rpart.plot)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path = 'C:/workspace2/processclimategrids/output/'
Tw <- rast(paste0(path, 'Tw.tif')); names(Tw)<-'Tw'
Tg <- rast(paste0(path, 'Bts.tif')); names(Tg)<-'Tg'
iucn.forest <- rast("output/iucn.forest.tif")

chm1 <- rast('data/chm/sdat_10023_1_20221208_174756168.tif')
chm2 <- rast('data/chm/sdat_10023_1_20221208_174835881.tif') 
chm3 <- rast('data/chm/sdat_10023_1_20221208_174901335.tif')
chm4 <- rast('data/chm/sdat_10023_1_20221208_174922722.tif')
chm5 <- rast('data/chm/sdat_10023_1_20221208_174943130.tif')

chm1 <-  chm1 %>% aggregate(fact=3, fun='mean', na.rm=TRUE) %>% project(Tw)
chm2 <-  chm2 %>% aggregate(fact=3, fun='mean', na.rm=TRUE) %>% project(Tw)
chm3 <-  chm3 %>% aggregate(fact=3, fun='mean', na.rm=TRUE) %>% project(Tw)
chm4 <-  chm4 %>% aggregate(fact=3, fun='mean', na.rm=TRUE) %>% project(Tw)
chm5 <-  chm5 %>% aggregate(fact=3, fun='mean', na.rm=TRUE) %>% project(Tw)

# writeRaster(chm1, 'data/chm/chm1.tif', overwrite=T)

chm <- mosaic(chm1, chm2, chm3, chm4, chm5, filename='data/chm/chm.tif', overwrite=T)
chm.a <- ifel(iucn.forest >= 100, chm, NA)
names(chm1) <- 'chm'
writeRaster(chm.a, 'data/chm/chm.tif', overwrite=T)

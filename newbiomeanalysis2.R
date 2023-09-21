library(sf)
library(terra)
library(ranger)
library(rpart)
library(rpart.plot)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path = 'C:/workspace2/processclimategrids/output/'
Tw <- rast(paste0(path, 'Tw.tif')); names(Tw)<-'Tw'
Twh <- rast(paste0(path, 'Twh.tif')); names(Twh)<-'Twh'
Tclx <- rast(paste0(path, 'Tclx.tif')); names(Tclx)<-'Tclx'
Tcl <- rast(paste0(path, 'Tcl.tif')); names(Tcl)<-'Tcl'
Tg <- rast(paste0(path, 'Bts.tif')); names(Tg)<-'Tg'
Tc <- rast(paste0(path, 'Tc.tif')); names(Tc)<-'Tc'
e <- rast(paste0(path, 'e.tif')); names(e)<-'e'
m <- rast(paste0(path, 'm.tif')); names(m)<-'m'
p <- rast(paste0(path, 'p.tif')); names(p)<-'p'
d <- rast(paste0(path, 'deficit.tif')); names(d)<-'d'
s <- rast(paste0(path, 'surplus.tif')); names(s)<-'s'
pAET <- rast(paste0(path, 'pAET.tif')); names(pAET)<-'pAET'
p3AET <- rast(paste0(path, 'p3AET.tif')); names(p3AET)<-'p3AET'
Elev5km = rast(paste0(path, 'Elev5km.tif'))
elev = rast(paste0(path, 'Elev5km.tif')) %>% project(Tw); names(elev)<-'elev'
Elev1km = rast(paste0(path, 'Elev1km.tif'))
#see 'newbiomanalysis.R' remarks for derivation of following layers.
hydric <- rast("output/hydric5km.tif"); names(hydric)<-'hydric'
sand <- rast("output/sand5km.tif"); names(sand)<-'sand'
clay <- rast("output/clay5km.tif"); names(clay)<-'clay'
soilpH <- rast("output/soilpH5km.tif"); names(soilpH)<-'soilpH'
bedrock <- rast("output/bedrock5km.tif"); names(bedrock)<-'bedrock'
marine <- rast('output/marine.tif'); names(marine)<-'marine'
sealevel <- rast('output/sealevel.tif'); names(sealevel)<-'sealevel'
sea <- rast('output/sea.tif'); names(sea)<-'sea'
chm <- rast('data/chm/chm.tif'); names(chm) <- 'chm'
slope <- rast("output/slope5km.tif"); names(slope)<-'slope'
rastbrick <- c(chm, Tw,Twh,Tg,Tc,Tclx,m,s,d,e,p3AET,slope, hydric, elev, sealevel,clay, sand,marine,soilpH,bedrock)


eco <- st_read('data/ecoregions.shp')
ecoalt <- read.csv('data/wwfeco.types.cover2.csv')

eco <- eco %>% subset(!BIOME %in% c(98))

ecoids <- eco$ECO_ID %>% unique()
for(i in 1:length(ecoids)){#i=326
  eco0 <- eco %>% subset(ECO_ID %in% ecoids[i]) %>% vect()
  ecopts0 <- spatSample(eco0, size=10, "random", strata=NULL)
  while(nrow(ecopts0) == 0){ecopts0 <- spatSample(eco0, size=10, "random", strata=NULL)}
  if(i==1){ecopts <- ecopts0}else{
    ecopts <- rbind(ecopts, ecopts0)} 
}
ecopts0 <- terra::spatSample(vect(eco), size=30000, "random", strata=NULL)
ecopts <- rbind(ecopts,ecopts0)
ecopts.repro <- ecopts %>% project(Tw)
ecopts.repro <- ecopts.repro %>% st_as_sf() %>%  left_join(ecoalt)

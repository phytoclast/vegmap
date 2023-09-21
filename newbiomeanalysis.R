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
# hydric1km <-  rast('C:/a/geo/global/hydric.tif'); names(hydric1km)<-'hydric'
# hydric <-  hydric1km %>% aggregate(fact=3, na.rm=T) %>% project(Tw, filename="output/hydric5km.tif", overwrite=TRUE)
hydric <- rast("output/hydric5km.tif"); names(hydric)<-'hydric'

# sand1km <-  rast('C:/a/geo/global/sand.tif'); names(sand1km)<-'sand'
# sand <-  sand1km %>% aggregate(fact=3, na.rm=T) %>% project(Tw, filename="output/sand5km.tif", overwrite=TRUE)
sand <- rast("output/sand5km.tif"); names(sand)<-'sand'
# clay1km <-  rast('C:/a/geo/global/clay.tif'); names(clay1km)<-'clay'
# clay <-  clay1km %>% aggregate(fact=3, na.rm=T) %>% project(Tw, filename="output/clay5km.tif", overwrite=TRUE)
clay <- rast("output/clay5km.tif"); names(clay)<-'clay'
# soilpH1km <-  rast('C:/a/geo/global/soilpH.tif'); names(soilpH1km)<-'soilpH'
# soilpH <-  soilpH1km %>% aggregate(fact=3, na.rm=T) %>% project(Tw, filename="output/soilpH5km.tif", overwrite=TRUE)
soilpH <- rast("output/soilpH5km.tif"); names(soilpH)<-'soilpH'
# bedrock1km <-  rast('C:/a/geo/global/bedrock.tif'); names(bedrock1km)<-'bedrock'
# bedrock <-  bedrock1km %>% aggregate(fact=3, na.rm=T) %>% project(Tw, filename="output/bedrock5km.tif", overwrite=TRUE)
bedrock <- rast("output/bedrock5km.tif"); names(bedrock)<-'bedrock'


# water10 <- rast('output/water10.tif')
# water20 <- rast('output/water20.tif')
# water50 <- rast('output/water50.tif')
# water100 <- rast('output/water100.tif'); names(water100)<-'water100'
# marine <- project(water100, Tw)
# writeRaster(marine, 'output/marine.tif')
marine <- rast('output/marine.tif'); names(marine)<-'marine'
# water200 <- rast('output/water200.tif')
# water500 <- rast('output/water500.tif')
# water1000 <- rast('output/water1000.tif')
# water2000 <- rast('output/water2000.tif')
# water = mean(water10,water20,water50,water100,water200,water500,water1000,water2000)
# elev <- ifel(Elev5km < 0, 0, Elev5km)
# sealevel <- elev/(elev+10)*-1+1
# sealevel <- project(sealevel, Tw)
# writeRaster(sealevel,'output/sealevel.tif')
sealevel <- rast('output/sealevel.tif'); names(sealevel)<-'sealevel'
# sea <- ifel( water10 > 0.1 &
#               water20 > 0.1 &
#               water50 > 0.3 &
#               water100 > 0.3 &
#               water200 > 0.1 &
#               water500 > 0.1 &
#               water1000 > 0.1 &
#               Elev5km < 10, 1, 0)
# sea <- sea %>% project(Tw)
# writeRaster(sea,'output/sea.tif',overwrite=T)
sea <- rast('output/sea.tif'); names(sea)<-'sea'
chm <- rast('data/chm/chm.tif'); names(chm) <- 'chm'
# slope1km <- Elev1km %>% terrain(v="slope", neighbors=8, unit="degrees", filename = 'output/slope1km.tif')
# slope5km <- slope1km %>% aggregate(fact=2, fun="mean", na.rm=TRUE, filename="output/slope5km.tif", overwrite=TRUE)
# slope5km <- slope5km %>% project(Tw,filename="output/slope5km.tif", overwrite=TRUE)
slope <- rast("output/slope5km.tif"); names(slope)<-'slope'
rastbrick <- c(chm, Tw,Twh,Tg,Tc,Tclx,m,s,d,e,p3AET,slope, hydric, elev, sealevel,clay, sand,marine,soilpH,bedrock)

iucnpath <- 'C:/a/geo/iucn/lvl1_frac_1km_ver004'

# iucn.forest <- rast(paste0(iucnpath,
# '/iucn_habitatclassification_fraction_lvl1__100_Forest__ver004.tif'))
# names(iucn.forest) <- 'iucn.forest'
# iucn.forest <- ifel(is.na(iucn.forest), 0,iucn.forest) %>% 
#   aggregate(fact=3, na.rm=T) %>% project(Tw, filename="output/iucn.forest.tif", overwrite=TRUE)
iucn.forest <- rast("output/iucn.forest.tif")

# iucn.desert <- rast(paste0(iucnpath, 
# '/iucn_habitatclassification_fraction_lvl1__800_Desert__ver004.tif'))
# names(iucn.desert) <- 'iucn.desert'
# iucn.desert <- ifel(is.na(iucn.desert), 0,iucn.desert) %>%
#   aggregate(fact=3, na.rm=T) %>% project(Tw, filename="output/iucn.desert.tif", overwrite=TRUE)
iucn.desert <- rast("output/iucn.desert.tif")

# iucn.rocky <- rast(paste0(iucnpath, 
# '/iucn_habitatclassification_fraction_lvl1__600_Rocky Areas__ver004.tif'))
# names(iucn.rocky) <- 'iucn.rocky'
# iucn.rocky <- ifel(is.na(iucn.rocky), 0,iucn.rocky) %>%
#   aggregate(fact=3, na.rm=T) %>% project(Tw, filename="output/iucn.rocky.tif", overwrite=TRUE)
iucn.rocky <- rast("output/iucn.rocky.tif")

# iucn.wetlands <- rast(paste0(iucnpath, 
# '/iucn_habitatclassification_fraction_lvl1__500_Wetlands inland__ver004.tif'))
# names(iucn.wetlands) <- 'iucn.wetlands'
# iucn.wetlands <- ifel(is.na(iucn.wetlands), 0,iucn.wetlands) %>% 
#   aggregate(fact=3, na.rm=T) %>% project(Tw, filename="output/iucn.wetlands.tif", overwrite=TRUE)
iucn.wetlands <- rast("output/iucn.wetlands.tif")

# iucn.artificial <- rast(paste0(iucnpath, 
# '/iucn_habitatclassification_fraction_lvl1__1400_Artificial - Terrestrial__ver004.tif'))
# names(iucn.artificial) <- 'iucn.artificial'
# iucn.artificial <- ifel(is.na(iucn.artificial), 0,iucn.artificial) %>%
#   aggregate(fact=3, na.rm=T) %>% project(Tw, filename="output/iucn.artificial.tif", overwrite=TRUE)
iucn.artificial <- rast("output/iucn.artificial.tif")
# iucn.forest.max <- iucn.forest / (1001 - (iucn.artificial+iucn.wetlands))
# iucn.forest.max <- focal(iucn.forest.max, w=5, fun='max')
# names(iucn.forest.max) <- 'iucn.forest.max'
# writeRaster(iucn.forest.max, 'output/iucn.forest.max.tif', overwrite=TRUE)
iucn.forest.max <- rast("output/iucn.forest.max.tif")


tnbarrens <- rast('data/tnbarrens_modified.tif')
tnbarrens.1 <-   ifel(tnbarrens$tnbarrens_modified_2 ==0, 1, 0)
newext <- ext(tnbarrens)+c(-20000,-10000,-10000,-100000)
tnbarrens.1 <- crop(tnbarrens.1, newext) %>% project(Tw)
names(tnbarrens.1) <- 'tnbarrens'



iucn.brick <- c(tnbarrens.1, iucn.forest, iucn.forest.max, iucn.desert, iucn.rocky, iucn.wetlands,iucn.artificial)



eco <- st_read('data/ecoregions.shp')
brown <- st_read('data/biotic_comm_la.shp')
kuchler <- st_read('data/kuchler_DD83.shp')
BPS <- rast('data/BPS.tif') #%>% project(Tw, method='near')
michigan <- sf::read_sf('data/michiganohio/MichLP_1800veg.shp')
ohio <- sf::read_sf('data/michiganohio/vege_orig_a_OH.shp')



MIalt <- read.csv('data/michiganohio/michiganlist.cover2.csv')
OHalt <- read.csv('data/michiganohio/ohiolist.cover2.csv')
brownalt <- read.csv('data/brown.types.cover2.csv')
kuchleralt <- read.csv('data/kuchler.types.cover2.csv')
ecoalt <- read.csv('data/wwfeco.types.cover2.csv')
bpsalt <- read.csv('data/bps.types.cover2.csv')
bpsrat <- foreign::read.dbf('data/BPS.dbf')
bpsrat <- bpsrat %>% left_join(bpsalt)
colnames(bpsrat)



# Michigan ----

michigan <- michigan %>% subset(!is.na(COVERTYPE))

miids <- michigan$COVERTYPE %>% unique()
for(i in 1:length(miids)){#i=326
  michigan0 <- michigan %>% subset(COVERTYPE %in% miids[i]) %>% vect()
  michiganpts0 <- spatSample(michigan0, size=10, "random", strata=NULL)
  while(nrow(michiganpts0) == 0){michiganpts0 <- spatSample(michigan0, size=5, "random", strata=NULL)}
  if(i==1){michiganpts <- michiganpts0}else{
    michiganpts <- rbind(michiganpts, michiganpts0)} 
}
michiganpts0 <- terra::spatSample(vect(michigan), size=2500, "random", strata=NULL)
michiganpts <- rbind(michiganpts,michiganpts0)
michiganpts.repro <- michiganpts %>% project(Tw)
michiganpts.repro <- michiganpts.repro %>% st_as_sf() %>%  left_join(MIalt)

# Ohio ----

ohio <- ohio %>% subset(!is.na(NAME))

ohids <- ohio$NAME %>% unique()
for(i in 1:length(ohids)){#i=326
  ohio0 <- ohio %>% subset(NAME %in% ohids[i]) %>% vect()
  ohiopts0 <- spatSample(ohio0, size=10, "random", strata=NULL)
  while(nrow(ohiopts0) == 0){ohiopts0 <- spatSample(ohio0, size=5, "random", strata=NULL)}
  if(i==1){ohiopts <- ohiopts0}else{
    ohiopts <- rbind(ohiopts, ohiopts0)} 
}
ohiopts0 <- terra::spatSample(vect(ohio), size=2500, "random", strata=NULL)
ohiopts <- rbind(ohiopts,ohiopts0)
ohiopts.repro <- ohiopts %>% project(Tw)
ohiopts.repro <- ohiopts.repro %>% st_as_sf() %>%  left_join(OHalt)

# BPS ----

BPS.pts <- BPS %>% as.data.frame(xy=T) %>% subset(!BPS %in% c(-9999, 11))
BPS.pts.s <- BPS.pts %>% group_by(BPS) %>% sample_n(size=10, replace = TRUE)
BPS.pts.s2 <- BPS.pts[sample(rownames(BPS.pts),size=10000),]
BPS.pts.s <- rbind(BPS.pts.s,BPS.pts.s2)
BPS.pts.sf <- BPS.pts.s %>% st_as_sf(coords = c('x', 'y'), crs=crs(BPS))
BPS.pts <- BPS.pts.sf %>% left_join(bpsrat[,
                                           c("VALUE","wet","vegcover","woodycover","treecover","ht" ,"NE","BE","DD","CD" )], by=c("BPS"="VALUE"))
BPS.pts.repro <- BPS.pts %>% sf::st_transform(crs = crs(Tw))


# kuchler ----

kuchlerids <- kuchler$TYPE %>% unique()
for(i in 1:length(kuchlerids)){#i=326
  kuchler0 <- kuchler %>% subset(TYPE %in% kuchlerids[i]) %>% vect()
  kuchlerpts0 <- spatSample(kuchler0, size=10, "random", strata=NULL)
  while(nrow(kuchlerpts0) == 0){kuchlerpts0 <- spatSample(kuchler0, size=5, "random", strata=NULL)}
  if(i==1){kuchlerpts <- kuchlerpts0}else{
    kuchlerpts <- rbind(kuchlerpts, kuchlerpts0)} 
}
kuchlerpts0 <- terra::spatSample(vect(kuchler), size=2500, "random", strata=NULL)
kuchlerpts <- rbind(kuchlerpts,kuchlerpts0)
kuchlerpts.repro <- kuchlerpts %>% project(eco)
kuchlerpts.repro <- kuchlerpts.repro %>% st_as_sf() %>%  left_join(kuchleralt)

# wwf ----
# eco1 <- eco %>% group_by(ECO_ID) %>% summarise()
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


# brown ----

# (brown$BIOMENAME1) %>% unique()
brown <- brown %>% subset(!BIOMENAME1 %in% c("Permanent Ice and Snow","Open Water Lakes"))
brownids <- brown$BIOMENAME1 %>% unique()
for(i in 1:length(brownids)){#i=326
  brown0 <- brown %>% subset(BIOMENAME1 %in% brownids[i]) %>% vect()
  brownpts0 <- spatSample(brown0, size=10, "random", strata=NULL)
  while(nrow(brownpts0) == 0){brownpts0 <- spatSample(brown0, size=10, "random", strata=NULL)}
  if(i==1){brownpts <- brownpts0}else{
    brownpts <- rbind(brownpts, brownpts0)} 
}
brownpts0 <- terra::spatSample(vect(brown), size=5000, "random", strata=NULL)
brownpts <- rbind(brownpts,brownpts0)
brownpts.repro <- brownpts %>% project(Tw)
brownpts.repro <- brownpts.repro %>% st_as_sf() %>%  left_join(brownalt)

#process layer sampling ----
commoncols <- c("wet","vegcover","woodycover","treecover","ht","NE","BE","DD","CD" )

ecopts.all <- ecopts.repro[,commoncols] %>% dplyr::bind_rows(BPS.pts.repro[,commoncols])%>% 
  dplyr::bind_rows(kuchlerpts.repro[,commoncols])%>% 
  dplyr::bind_rows(brownpts.repro[,commoncols])%>% 
dplyr::bind_rows(michiganpts.repro[,commoncols])%>% 
  dplyr::bind_rows(ohiopts.repro[,commoncols])

 # plot(vect(ecopts.all))
 

# reex <- BPS.pts.repro %>% st_crop(y=c(xmin= -110, ymin= 40, xmax= -101, ymax=45))



ecopts1 <- rastbrick %>% extract(ecopts.all)
ecopts1a <- iucn.brick %>% extract(ecopts.all)
ecopts2 <- ecopts.all %>% st_drop_geometry()
ecopts3 <- cbind(ecopts2, ecopts1, ecopts1a)
# ecopts3 <- ecopts3[,-26]
# ecopts3 <- ecopts3 %>% left_join(ecoalt[,c('ECO_ID','altbiome','altbiome2')])
ecopts3 <- subset(ecopts3)


ecopts3 <- ecopts3 %>% mutate(
  barren = iucn.desert+iucn.rocky,
  wet = pmin(1,wet+iucn.wetlands/1000),
  vegcover = vegcover*((barren/1000)*-1+1),
  woodycover = woodycover*((barren/1000)*-1+1),
  treecover = treecover*((barren/1000)*-1+1),
  #ht = ht*(1-((barren/1000)*-1+1)/2),
  vegcover = ifelse(is.na(tnbarrens),vegcover,  vegcover*(1-tnbarrens*0.05)),
  woodycover = ifelse(is.na(tnbarrens),woodycover,  woodycover*(1-tnbarrens*0.75)),
  treecover = ifelse(is.na(tnbarrens),treecover,  treecover*(1-tnbarrens*0.75)),
  treecover = ifelse(Tg < 9,treecover*iucn.forest.max, treecover),
  chm = ifelse(Tg < 9 & !is.na(chm), pmin(Tg+3*Tg/7, chm), chm),
  chm = ifelse(barren > 100 & is.na(chm), 0, chm),
  chm = ifelse(barren > 900 & !is.na(chm), chm/2, chm),
  chm = ifelse(Tg < 9 & !is.na(chm), pmin(Tg+3*Tg/6,chm), chm)
  # chm = 1.308882*chm,
  # chm = ifelse(!is.na(ht), (chm+ht)/2,chm)
  # chm = ifelse(ht >=50 & ht < 70, chm*1, ifelse(ht >=70, chm*2, chm)),
  # chm = ifelse(!is.na(chm)&!is.na(ht)& ht >=22, (chm+ht)/2, chm)
  #ht = ifelse(Tg < 9,ht*(1-iucn.forest.max/2), ht)
)
rastbrick <- c(Tw,Twh,Tg,Tc,Tclx,m,s,d,e,p3AET,slope, hydric, elev, sealevel,clay, sand,marine,soilpH,bedrock)
st_write(ecopts.all, 'output/ecopts.all.shp', append=FALSE)

# wet 29463*.002 ----
ecopts3.wet <- subset(ecopts3,!is.na(wet) & 
                        !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))
library(grf)
# bf <- boosted_regression_forest(ecopts3.wet[,c("Tw","Twh","Tg","Tc","Tclx","m","s","d","e","p3AET",
#                     "elev","slope","hydric","sealevel","clay","sand","marine","soilpH","bedrock")],
#                     ecopts3.wet$wet,
#                                 num.trees=50, sample.fraction = 0.5)
llf <- ll_regression_forest(ecopts3.wet[,c("Tw","Twh","Tg","Tc","Tclx","m","s","d","e","p3AET",
                                               "elev","slope","hydric","sealevel","clay","sand","marine","soilpH","bedrock")],
                                ecopts3.wet$wet,
                                num.trees=50, sample.fraction = 0.5)

# 
# rf <- ranger(wet ~ 
#                Tw+Twh+Tg+Tc+Tclx+m+s+d+e+p3AET+
#                elev+slope+hydric+sealevel+clay+sand+marine+soilpH+bedrock
#              ,
#              data=ecopts3.wet, num.trees=50, sample.fraction = 0.5, max.depth = 12, importance = 'impurity', write.forest = TRUE)
wetmodel <- predict(object=rastbrick,  model=llf, na.rm=TRUE)
# wetmodel <- predict(object=rastbrick,  model=bf, na.rm=TRUE)
# wetmodel <- predict(object=rastbrick,  model=rf, na.rm=TRUE)
wetmodel <- wetmodel$predictions
plot(wetmodel)
names(wetmodel) <- 'wetmodel'
wetmodel <- extend(wetmodel, Tw); wetmodel <- crop(wetmodel, Tw)

writeRaster(wetmodel, 'output/global/llwetmodel.tif', overwrite=T)
# writeRaster(wetmodel, 'output/global/boostedwetmodel.tif', overwrite=T)
# writeRaster(wetmodel, 'output/global/boostedwetmodel.tif', overwrite=T)
wetmodel <- rast('output/global/wetmodel.tif')
artificial.null <- ifel(iucn.artificial >0,0,0)
rastbrick <- c(artificial.null, wetmodel,Tw,Twh,Tg,Tc,Tclx,m,s,d,e,p3AET,slope, hydric, elev, sealevel,clay, sand,marine,soilpH,bedrock)

ecopts1b <- wetmodel %>% extract(ecopts.all)
ecopts4 <- cbind(ecopts3, ecopts1b)

# canopy height lidar ----
ecopts3.ht <- subset(ecopts4,!is.na(chm) &!is.na(wetmodel) & #(chm < Tg*3 | Tg >= 9) &
                       !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))
# ecopts3.veg <- ecopts3.veg %>% mutate(vegcover = ifelse(Tg < 1.5, 0, vegcover))
library(grf)
bf <- boosted_regression_forest(ecopts3.ht[,c("wetmodel","iucn.artificial","Tw","Twh","Tg",
                                              "Tc","Tclx","m","s","d","e","p3AET",
             "slope","hydric","sealevel","clay","sand","marine","soilpH","bedrock")],
             ecopts3.ht$chm,
             num.trees=50, sample.fraction = 0.1)


rf <- ranger(chm ~ wetmodel+iucn.artificial+
               Tw+Twh+Tg+Tc+Tclx+m+s+d+e+p3AET+
               slope+hydric+sealevel+clay+sand+marine+soilpH+bedrock
             ,
             data=ecopts3.ht, num.trees=50, sample.fraction = 0.1, max.depth = 18, importance = 'impurity', write.forest = TRUE)

htmodel <- predict(object=rastbrick,  model=rf, na.rm=TRUE)
htmodel <- htmodel$predictions
plot(htmodel)
names(htmodel) <- 'chmodel'
htmodel <- extend(htmodel, Tw); htmodel <- crop(htmodel, Tw)

writeRaster(htmodel, 'output/global/chmodel.tif', overwrite=T)
# library(ggplot2)
# gplt <- ggplot(subset(ecopts3.ht, m > 1))+
#   geom_smooth(aes(x=Tg, y=chm))
# gplt
# gplt <- ggplot(subset(ecopts3, !is.na(ht) & !is.na(chm)))+
#   geom_smooth(aes(y=ht, x=chm))
# gplt
# mod <- lm(ht~chm+0, data=subset(ecopts3, !is.na(ht) & !is.na(chm)))
# summary(mod)
# 

# canopy height ----
ecopts3.ht <- subset(ecopts4,!is.na(ht) &!is.na(wetmodel) &
                       !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))
# ecopts3.veg <- ecopts3.veg %>% mutate(vegcover = ifelse(Tg < 1.5, 0, vegcover))

rf <- ranger(ht ~ wetmodel+
               Tw+Twh+Tg+Tc+Tclx+m+s+d+e+p3AET+
               slope+hydric+sealevel+clay+sand+marine+soilpH+bedrock
             ,
             data=ecopts3.ht, num.trees=50, sample.fraction = 0.1, max.depth = 18, importance = 'impurity', write.forest = TRUE)

htmodel <- predict(object=rastbrick,  model=rf, na.rm=TRUE)
htmodel <- htmodel$predictions
plot(htmodel)
names(htmodel) <- 'htmodel'
htmodel <- extend(htmodel, Tw); htmodel <- crop(htmodel, Tw)

writeRaster(htmodel, 'output/global/htmodel.tif', overwrite=T)

# veg 29463*.2 1118803*0.02 ----
ecopts3.veg <- subset(ecopts4,!is.na(vegcover)&!is.na(wetmodel) &
                        !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))
# ecopts3.veg <- ecopts3.veg %>% mutate(vegcover = ifelse(Tg < 1.5, 0, vegcover))

rf <- ranger(vegcover ~ wetmodel+
               Tw+Twh+Tg+Tc+Tclx+m+s+d+e+p3AET+
               slope+hydric+sealevel+clay+sand+marine+soilpH+bedrock
             ,
             data=ecopts3.veg, num.trees=50, sample.fraction = 0.1, max.depth = 18, importance = 'impurity', write.forest = TRUE)

vegmodel <- predict(object=rastbrick,  model=rf, na.rm=TRUE)
vegmodel <- vegmodel$predictions
plot(vegmodel)
names(vegmodel) <- 'vegcover'
vegmodel <- extend(vegmodel, Tw); vegmodel <- crop(vegmodel, Tw)

writeRaster(vegmodel, 'output/global/vegmodel.tif', overwrite=T)

# rastbrick <- c(wetmodel,vegmodel,Tw,Twh,Tg,Tc,Tclx,m,s,d,e,p3AET,slope, hydric, elev, sealevel,clay, sand,marine,soilpH,bedrock)

# wood ----
ecopts3.wood <- subset(ecopts4,!is.na(wet)&!is.na(woodycover)&!is.na(wetmodel) &
                         !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))
# ecopts3.wood <- ecopts3.wood %>% mutate(woodycover = ifelse(Tg < 4.5, 0, woodycover))

rf <- ranger(woodycover ~ wetmodel+
               Tw+Twh+Tg+Tc+Tclx+m+s+d+e+p3AET+
               slope+hydric+sealevel+clay+sand+marine+soilpH+bedrock
             ,
             data=ecopts3.wood, num.trees=50, sample.fraction = 0.1, max.depth = 18, importance = 'impurity', write.forest = TRUE)

woodmodel <- predict(object=rastbrick,  model=rf, na.rm=TRUE)
woodmodel <- woodmodel$predictions
plot(woodmodel)
names(woodmodel) <- 'woodycover'
woodmodel <- extend(woodmodel, Tw); woodmodel <- crop(woodmodel, Tw)

writeRaster(woodmodel, 'output/global/woodmodel.tif', overwrite=T)


# rastbrick <- c(wetmodel,vegmodel,woodmodel,Tw,Twh,Tg,Tc,Tclx,m,s,d,e,p3AET,slope, hydric, elev, sealevel,clay, sand,marine,soilpH,bedrock)

# tree ----
ecopts3.tree <- subset(ecopts4,!is.na(wet)&!is.na(treecover)&!is.na(wetmodel) &
                         !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))
# ecopts3.tree <- ecopts3.tree %>% mutate(treecover = ifelse(Tg < 4.5, 0, treecover))

rf <- ranger(treecover ~ wetmodel+
               Tw+Twh+Tg+Tc+Tclx+m+s+d+e+p3AET+
               slope+hydric+sealevel+clay+sand+marine+soilpH+bedrock
             ,
             data=ecopts3.tree, num.trees=50, sample.fraction = 0.1, max.depth = 18, importance = 'impurity', write.forest = TRUE)

treemodel <- predict(object=rastbrick,  model=rf, na.rm=TRUE)
treemodel <- treemodel$predictions
plot(treemodel)
names(treemodel) <- 'treecover'
treemodel <- extend(treemodel, Tw); treemodel <- crop(treemodel, Tw)

writeRaster(treemodel, 'output/global/treemodel.tif', overwrite=T)

# rastbrick <- c(wetmodel,vegmodel,woodmodel, treemodel,Tw,Twh,Tg,Tc,Tclx,m,s,d,e,p3AET,slope, hydric, elev, sealevel,clay, sand,marine,soilpH,bedrock)

# NE ----
ecopts3.NE <- subset(ecopts4,!is.na(NE)&Tg>=3 &!is.na(wetmodel) &
                       !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))

rf <- ranger(NE ~ wetmodel+
               Tw+Twh+Tg+Tc+Tclx+m+s+d+e+p3AET+
               slope+hydric+sealevel+clay+sand+marine+soilpH+bedrock
             ,
             data=ecopts3.NE, num.trees=50, sample.fraction = 0.1, max.depth = 18, importance = 'impurity', write.forest = TRUE)

NEver <- predict(object=rastbrick,  model=rf, na.rm=TRUE)
NEver <- NEver$predictions
plot(NEver)
names(NEver) <- 'NE'
NEver <- extend(NEver, Tw); NEver <- crop(NEver, Tw)

writeRaster(NEver, 'output/global/NEver.tif', overwrite=T)
# BE ----
ecopts3.BE <- subset(ecopts4,!is.na(BE)&Tg>=3 &!is.na(wetmodel) &
                       !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))

rf <- ranger(BE ~ wetmodel+
               Tw+Twh+Tg+Tc+Tclx+m+s+d+e+p3AET+
               slope+hydric+sealevel+clay+sand+marine+soilpH+bedrock
             ,
             data=ecopts3.BE, num.trees=50, sample.fraction = 0.1, max.depth = 18, importance = 'impurity', write.forest = TRUE)

BEver <- predict(object=rastbrick,  model=rf, na.rm=TRUE)
BEver <- BEver$predictions
plot(BEver)
names(BEver) <- 'BE'
BEver <- extend(BEver, Tw); BEver <- crop(BEver, Tw)

writeRaster(BEver, 'output/global/BEver.tif', overwrite=T)
# DD ----
ecopts3.DD <- subset(ecopts4,!is.na(DD)&Tg>=3 &!is.na(wetmodel) &
                       !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))

rf <- ranger(DD ~ wetmodel+
               Tw+Twh+Tg+Tc+Tclx+m+s+d+e+p3AET+
               slope+hydric+sealevel+clay+sand+marine+soilpH+bedrock
             ,
             data=ecopts3.DD, num.trees=50, sample.fraction = 0.1, max.depth = 18, importance = 'impurity', write.forest = TRUE)

DDeci <- predict(object=rastbrick,  model=rf, na.rm=TRUE)
DDeci <- DDeci$predictions
plot(DDeci)
names(DDeci) <- 'DD'
DDeci <- extend(DDeci, Tw); DDeci <- crop(DDeci, Tw)

writeRaster(DDeci, 'output/global/DDeci.tif', overwrite=T)

# CD ----
ecopts3.CD <- subset(ecopts4,!is.na(CD)&Tg>=3 &!is.na(wetmodel) &
                       !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))

rf <- ranger(CD ~ wetmodel+
               Tw+Twh+Tg+Tc+Tclx+m+s+d+e+p3AET+
               slope+hydric+sealevel+clay+sand+marine+soilpH+bedrock
             ,
             data=ecopts3.CD, num.trees=50, sample.fraction = 0.1, max.depth = 18, importance = 'impurity', write.forest = TRUE)

CDeci <- predict(object=rastbrick,  model=rf, na.rm=TRUE)
CDeci <- CDeci$predictions
plot(CDeci)
names(CDeci) <- 'CD'
CDeci <- extend(CDeci, Tw); CDeci <- crop(CDeci, Tw)

writeRaster(CDeci, 'output/global/CDeci.tif', overwrite=T)

wetmodel <- rast('output/global/wetmodel.tif')
vegmodel <- rast('output/global/vegmodel.tif')
woodmodel <- rast('output/global/woodmodel.tif')
treemodel <- rast('output/global/treemodel.tif')
NEver <- rast('output/global/NEver.tif')
BEver <- rast('output/global/BEver.tif')
DDeci <- rast('output/global/DDeci.tif')
CDeci <- rast('output/global/CDeci.tif')
htmodel <- rast('output/global/htmodel.tif')
chmodel <- rast('output/global/chmodel.tif')
ecopts1b <- c(chmodel,htmodel, wetmodel) %>% extract(ecopts.all)
ecopts4 <- cbind(ecopts3, ecopts1b)

NEwood <- woodmodel*NEver
BEwood <- woodmodel*BEver
DDwood <- woodmodel*DDeci
CDwood <- woodmodel*CDeci
writeRaster(NEwood, 'output/global/NEwood.tif', overwrite=T)
writeRaster(BEwood, 'output/global/BEwood.tif', overwrite=T)
writeRaster(DDwood, 'output/global/DDwood.tif', overwrite=T)
writeRaster(CDwood, 'output/global/CDwood.tif', overwrite=T)

grass <- vegmodel - min(vegmodel, woodmodel)
writeRaster(grass, 'output/global/grassmodel.tif', overwrite=T)
shrub <- woodmodel - min(treemodel, woodmodel)
writeRaster(shrub, 'output/global/shrubmodel.tif', overwrite=T)


soilmin <- ifel(d > s,d*0,  max(d*0, (150-d)/150*100))
soilmax <- ifel(s > d,d*0+100,  min(d*0+100, s/150*100))
writeRaster(soilmax, 'output/global/soilmax.tif', overwrite=T)
writeRaster(soilmin, 'output/global/soilmin.tif', overwrite=T)
















ht1 <- (htmodel+chmodel)/2
ht2 <- treemodel*ht1/100+(woodmodel-treemodel)*3/100+(vegmodel-woodmodel)*0.5/100
# ht2 <- ifel(treemodel >= 25 & Tg >= 6, ht2, ifel(woodmodel >= 50, 5, ifel(woodmodel >= 25 & vegmodel >= 50,3, ifel(woodmodel >= 5 & vegmodel >= 25, 1, 0)) ))
writeRaster(ht2, 'output/global/ht2.tif', overwrite=T)


# biom.t <- ifel(Tclx >= 0 & Tc >= 15 & Tg >= 18 | (DDeci > 0.25 & Tc >= 12 & CDeci < 0.25), 1,
#                ifel(Tclx >= -10 & Tc >= 0 | (BEver > 0.25 & Tclx >= -15 & CDeci < 0.25), ifel(Tg >= 18, 2,ifel(Tg >= 6,3,4)),
#                     ifel(Tg >= 12 & (NEver < 0.67 | Tw >= 18), 5,ifel(Tg >= 6,6,7))))
biom.t <- ifel(Tclx >= 0 & Tc >= 15 & Tg >= 18, 1,
               ifel(Tclx >= -15 & Tc >= 0, ifel(Tg >= 18, 2,ifel(Tg >= 6,3,4)),
                    ifel(Tg >= 12, 5,ifel(Tg >= 6,6,7))))
biom.t <- ifel(Tclx >= 0 & Tc >= 12, ifel(Tg >= 24,1,2),
              ifel(Tclx >= -15 & Tc >= 0, ifel(Tg >= 18, 3,ifel(Tg >= 6,4,5)),
                   ifel(Tg >= 12, 6,ifel(Tg >= 6,7,8))))
Tcc1215 <- min(Tclx*0.8+12,Tc)

Tsummer <-  ifel(Tg >= 24, 1, ifel(Tg >= 18, 2,ifel(Tg >= 12, 3,ifel(Tg >= 6, 4,5))))
Twinter <-  ifel(Tcc1215 >= 12, 10,ifel(Tcc1215 >= 0, 20,30))

biom.t2 <-  Tsummer+Twinter

writeRaster(biom.t2, 'output/global/biom.t2.tif', overwrite=T)

m.peak <-  ifel(p3AET >= 200, 20,10)
m.def <-  ifel(d >= 200, 2,1)
m.ratio <- ifel(m >= 1.2, 500, ifel(m >= 0.6, 400, ifel(m >= 0.35, 300, ifel(m >= 0.2, 200,100))))
m.s <-  m.ratio + m.def + m.peak
writeRaster(m.s, 'output/global/m.s.tif', overwrite=T)


ms.max <-  s - ifel(d > 200, 200, d)
md.max <-  d - ifel(s > 200, 200, s)
# ms.max2 <-  s - ifel(d > 150, 150, d)
# md.max2 <-  d - ifel(s > 150, 150, s)
m.s   <- ifel(ms.max > 0 | m >=1, ifel(md.max > 0,200,300),100)
m.s <- m.s + m.peak
# m.s2   <- ifel(ms.max2 > 0 | m >=1, ifel(md.max2 > 0,2,3),1)
writeRaster(ms.max, 'output/global/ms.max.tif', overwrite=T)
writeRaster(md.max, 'output/global/md.max.tif', overwrite=T)
writeRaster(m.s, 'output/global/m.s.tif', overwrite=T)
# writeRaster(m.s2, 'output/global/m.s2.tif', overwrite=T)

writeRaster(biom.t, 'output/global/biom.t0.tif', overwrite=T)
# biom.t2 <- ifel(Tclx >= 0 & Tc >= 12, ifel(Tw >= 18, 1, 2),
#                ifel(Tclx >= -12 & Tc >= 0, ifel(Tw >= 18, 3,ifel(Tg >= 6,4,5)),
#                     ifel(Tw >= 18, 6,ifel(Tg >= 6,7,8))))
# 
# plot(biom.t2)
# writeRaster(biom.t2, 'output/global/biom.t2.tif', overwrite=T)


ever <- (BEver+NEver)/(BEver+DDeci+CDeci+NEver)
# ever.1 <- ifel(ever >= 0.67, 1, ifel(ever > 0.33, 2,3))
ever.1 <- ifel(ever >= .5, 1,2)
# plot(ever.1)
# plot(BEver/4+DDeci > 0.10)

biomeveg <- ifel(Tg < 6 | (treemodel < 10 & Tg < 9), 600, ifel(wetmodel >= 0.5,ifel(woodmodel >= 25, 700,800),
                                                               ifel(treemodel >= 67 | (Tg < 9 & treemodel >= 25), 500,
                                   ifel(treemodel >= 25 & treemodel >= shrub, 400,
                                        ifel(vegmodel >= 50, ifel(shrub >= grass | shrub >= 25, 300,200),100)))))
plot(biomeveg)
writeRaster(biomeveg, 'output/global/biomeveg.tif', overwrite=T)

biomeveg.e <- ifel(biomeveg %in% c(300,400,500), biomeveg+ever.1*10, biomeveg)
writeRaster(biomeveg.e, 'output/global/biomeveg.e.tif', overwrite=T)

biomeveg.t <- biomeveg.e + biom.t
writeRaster(biomeveg.t, 'output/global/biomeveg.t.tif', overwrite=T)



ecopts3.ht <- subset(ecopts4,!is.na(htmodel)& treecover >=25 & !is.na(wetmodel) & #Tg >= 6 & Tg < 12 & m >= 2 &
                       !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))
ecopts3.ht <- ecopts3.ht %>% mutate(ht1 = htmodel+chmodel/2, ht2 = treecover*ht1/100+(woodycover-treecover)*3/100,#+(vegcover-woodycover)*0.5/100,
                                    Tcc = pmin(Tc, Tclx+15), d150 = d - pmin(s,050),s150 = s - pmin(d,150), 
                                    rd150 = (d - pmin(s,050))/(e+1),rs150 = (s - pmin(d,150))/(e+1),
                                    soilmin = ifelse(d > s,d*0,  pmax(d*0, (150-d)/150*100)),
                                    soilmax = ifelse(s > d,d*0+100,  pmin(d*0+100, s/150*100)),
                                    mtrans = m/(1+m), mtrans2 = m/(0.333+m), mlog = log(m+0.01))

cortab<-  cor(ecopts3.ht[ecopts3.ht$Tg >=12 ,c("chmodel","ht","ht2","Tg","m","s","Tw","Twh","Tc","Tclx","d150","s150","rd150","rs150",
                            "d","p3AET","slope","hydric","sand","sealevel","marine","soilmin","soilmax","mtrans","mtrans2","mlog")], use = "pairwise.complete.obs") %>% as.data.frame()

rp <- rpart(ht ~ 
              Tg+m+Tw+Twh+soilpH+d+Tclx+Tcc+Tc+#s+d150+s150+rd150+rs150+sealevel+
              p3AET+slope+hydric+sand+marine,
            data = ecopts3.ht,  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/ht.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp) # Make plot
dev.off()
rp <- rpart(ht2 ~ 
              Tg+m+Tw+Twh+soilpH+d+Tclx+Tcc+Tc+#s+d150+s150+rd150+rs150+sealevel+
              p3AET+slope+hydric+sand+marine,
            data = ecopts3.ht,  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/ht2.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp) # Make plot
dev.off()

rp <- rpart(chmodel ~ 
              Tg+m+Tw+Twh+soilpH+d+Tclx+Tcc+Tc+#s+d150+s150+rd150+rs150+sealevel+
              p3AET+slope+hydric+sand+marine,
            data = ecopts3.ht,  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/chm.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp) # Make plot
dev.off()





ecopts3.NE <- subset(ecopts4,!is.na(NE)& woodycover >=60 & !is.na(wetmodel) &
                       !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))
ecopts3.NE <- ecopts3.NE %>% mutate(ht2 = htmodel+chmodel/2,
                                    Tcc = pmin(Tc, Tclx+15), d150 = d - pmin(s,050),s150 = s - pmin(d,150), 
                                    rd150 = (d - pmin(s,050))/(e+1),rs150 = (s - pmin(d,150))/(e+1))


rp <- rpart(NE ~ 
              Tg+m+s+Tw+Twh+Tc+Tclx+Tcc+d150+s150+rd150+rs150+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts3.NE,  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/NE.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp) # Make plot
dev.off()

rp <- rpart(BE ~ 
              Tg+m+s+Tw+Twh+Tc+Tclx+Tcc+d150+s150+rd150+rs150+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts3.NE,  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/BE.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp) # Make plot
dev.off()

ecopts3.DD <- subset(ecopts4,!is.na(NE) & woodycover >=25 & DD+CD+NE+BE > 0.5 & !is.na(wetmodel) & Tc > 12 &
                       !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))

ecopts3.DD <- ecopts3.DD %>% mutate(DB = DD/(DD+BE), DDCD = DD/(DD+CD), 
                                    Tcc12 = pmin(Tc, Tclx+12),Tcc15 = pmin(Tc, Tclx+15),Tcc18 = pmin(Tc, Tclx+18),tropic = (DD+BE)/(DD+CD+BE+NE))

rp <- rpart(tropic ~ 
              Tg+m+s+Tw+Twh+Tc+Tclx+Tcc12+Tcc15+Tcc18+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts3.DD,  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/tropic.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp) # Make plot
dev.off()

ecopts3.DD <- subset(ecopts4,!is.na(NE) & woodycover >=25 & DD+BE > 0.5 & !is.na(wetmodel) & Tc > 12 &
                       !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))

ecopts3.DD <- ecopts3.DD %>% mutate(ED = (BE+NE)/(DD+BE+NE+CD), DB = DD/(DD+BE), DDCD = DD/(DD+CD), Tcc1215 = pmin(Tc,Tclx*0.8 +15),
                                    Tcc12 = pmin(Tc, Tclx+12),Tcc15 = pmin(Tc, Tclx+15),Tcc18 = pmin(Tc, Tclx+18),
                                    tropic = (DD+BE)/(DD+CD+BE+NE), d150 = d - pmin(s,050),s150 = s - pmin(d,150), 
                                    rd150 = (d - pmin(s,050))/(e+1),rs150 = (s - pmin(d,150))/(e+1),
                                    mtrans = m/(1+m), mtrans2 = m/(0.333+m), mlog = log(m+0.01))

 cortab<-  cor(ecopts3.DD[ ,c("ED","DB","DDCD","tropic", "Tg","m","s","Tw","Twh","Tc","Tclx","Tcc12","Tcc15","Tcc18","d150","s150","rd150","rs150",
                    "d","p3AET","slope","hydric","sand","sealevel","marine","mtrans","mtrans2","mlog","Tcc1215")],  use = "pairwise.complete.obs") %>% as.data.frame()

rp <- rpart(DB ~ 
              Tg+m+s+Tw+Twh+Tc+Tclx+Tcc12+Tcc15+Tcc18+d150+s150+rd150+rs150+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts3.DD,  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/DB.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp) # Make plot
dev.off()
rp <- rpart(CD ~ 
              Tg+m+s+Tw+Twh+Tc+Tclx+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts3.NE,  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/CD.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp) # Make plot
dev.off()


ecopts3.tree <- subset(ecopts4,!is.na(treecover)  & !is.na(wetmodel) &
                       !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))
ecopts3.tree <- ecopts3.tree %>% mutate(ht2 = htmodel+chmodel/2,
                                    Tcc = pmin(Tc, Tclx+15), d150 = d - pmin(s,050),s150 = s - pmin(d,150), 
                                    rd150 = (d - pmin(s,050))/(e+1),rs150 = (s - pmin(d,150))/(e+1),
                                    soilmin = ifelse(d > s,d*0,  pmax(d*0, (150-d)/150*100)),
                                    soilmax = ifelse(s > d,d*0+100,  pmin(d*0+100, s/150*100)),
                                    mtrans = m/(1+m), mtrans2 = m/(0.333+m), mlog = log(m+0.01))

cortab<-  cor(ecopts3.tree[ecopts3.tree$Tg >= 12,c("treecover","woodycover","vegcover","chmodel","ht","ht2","Tg","m","s","Tw","Twh","Tc","Tclx","d150","s150","rd150","rs150",
                                               "d","p3AET","slope","hydric","sand","sealevel","marine","soilmin","soilmax","mtrans","mtrans2","mlog")],
              use = "pairwise.complete.obs") %>% as.data.frame()


rp <- rpart(treecover ~ 
              Tg+m+s+Tw+Twh+Tc+Tclx+Tcc+d150+s150+rd150+rs150+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts3.tree,  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/treecover.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp) # Make plot
dev.off()

ecopts3.tree <- subset(ecopts4,!is.na(treecover)  & !is.na(wetmodel) & 
                         !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))
ecopts3.tree <- ecopts3.tree %>% mutate(ht2 = htmodel+chmodel/2,
                                        Tcc = pmin(Tc, Tclx+15), s050 = s - pmin(d,050),s150 = s - pmin(d,150))


rp <- rpart(treecover ~ 
              Tg+m+s+Tw+Twh+Tc+Tclx+Tcc+s050+s150+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts3.tree,  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/treecover.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp) # Make plot
dev.off()

rp <- rpart(woodycover ~ 
              Tg+m+s+Tw+Twh+Tc+Tclx+Tcc+s050+s150+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts3.tree,  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/woodycover.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp) # Make plot
dev.off()

rp <- rpart(vegcover ~ 
              Tg+m+s+Tw+Twh+Tc+Tclx+Tcc+s050+s150+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts3.tree,  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/vegcover.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp) # Make plot
dev.off()

ecopts3.shrub <- subset(ecopts4,!is.na(treecover)  & !is.na(wetmodel) & treecover < 50 & vegcover > 80 & Tg >= 12 &
                          !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))
ecopts3.shrub <- ecopts3.shrub %>% mutate(ht2 = htmodel+chmodel/2,
                                          Tcc = pmin(Tc, Tclx+15), s050 = s - pmin(d,050),s150 = s - pmin(d,150))
rp <- rpart(woodycover ~ 
              Tg+m+s+Tw+Twh+Tc+Tclx+Tcc+s050+s150+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts3.shrub,  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/notreewoodycover.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp) # Make plot
dev.off()

ecopts3.grass <- subset(ecopts4,!is.na(treecover)  & !is.na(wetmodel) &
                          !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))
ecopts3.grass <- ecopts3.grass %>% mutate(ht2 = htmodel+chmodel/2,
                                          Tcc = pmin(Tc, Tclx+15), s050 = s - pmin(d,050),s150 = s - pmin(d,150), shrub = woodycover- treecover, grass = vegcover - woodycover)
rp <- rpart(grass ~ 
              Tg+m+s+Tw+Twh+Tc+Tclx+Tcc+s050+s150+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts3.grass,  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/grasscover.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp) # Make plot
dev.off()
rp <- rpart(shrub ~ 
              Tg+m+s+Tw+Twh+Tc+Tclx+Tcc+s050+s150+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts3.grass,  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/shrubcover.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp) # Make plot
dev.off()


# montane ----
ecosummary2 <-  read.csv('output/ecosummary2.csv')
eco <- st_read('data/ecoregions.shp')
eco <- eco %>% subset(!BIOME %in% c(98))

ecoids <- eco$ECO_ID %>% unique()
for(i in 1:length(ecoids)){#i=326
  eco0 <- eco %>% subset(ECO_ID %in% ecoids[i]) %>% vect()
  ecopts0 <- spatSample(eco0, size=10, "random", strata=NULL)
  while(nrow(ecopts0) == 0){ecopts0 <- spatSample(eco0, size=10, "random", strata=NULL)}
  if(i==1){ecopts <- ecopts0}else{
    ecopts <- rbind(ecopts, ecopts0)} 
}
ecopts0 <- terra::spatSample(vect(eco), size=100000, "random", strata=NULL)
ecopts <- rbind(ecopts,ecopts0)
ecopts.repro <- ecopts %>% project(Tw)
ecopts.montane <- ecopts.repro %>% st_as_sf() %>% left_join(ecosummary2)
unique(ecopts.montane$biome2)

ecopts.montane.0 <- subset(ecopts.montane, biome2 %in% c("boreal","cold","oceanic","warm","subtropical","tropical","montane"))

rastbrick <- c(chm, Tw,Twh,Tg,Tc,Tclx,m,s,d,e,p3AET,slope, hydric, elev, sealevel,clay, sand,marine,soilpH,bedrock)

ecopts.montane.1 <- rastbrick %>% extract(ecopts.montane.0)
ecopts.montane.2 <- cbind(st_drop_geometry(ecopts.montane.0), ecopts.montane.1)
ecopts.montane.2 <- subset(ecopts.montane.2, biome2 %in% c("boreal","cold","oceanic","warm","subtropical","tropical","montane"),
                           !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m) & Tg >= 6)
ecopts.montane.2 <- ecopts.montane.2 %>% group_by(biome2) %>% mutate(ct = length(biome2), wt=1/(ct+1))
ecopts.montane.2 <- ecopts.montane.2 %>% mutate(warm = ifelse(biome2 %in% c( 'boreal'), 'cold', 
            ifelse(biome2 %in% c('warm','oceanic', 'subtropical','tropical', 'montane'),'warm', biome2)), 
               tropic = ifelse(biome2 %in% c('warm','oceanic', 'subtropical','boreal'), 'cold', 
                                                                ifelse(biome2 %in% c('tropical', 'montane'),'warm', biome2)), 
                Tcc15 = pmin(Tc,Tclx +15),
                Tcc12 = pmin(Tc,Tclx +12), 
                Tcc14 = pmin(Tc,Tclx +14), 
                Tcc1215 = pmin(Tc,Tclx*0.8 +15),
                Tcc913 = pmin(Tc,Tclx*0.9231 +13.8462))
unique(ecopts.montane.2$warm)
ecopts.montane.2 <- ecopts.montane.2 %>% mutate(
  tropicaler = ifelse(biome2 %in% c('tropical', 'montane'),1,0),
  warmer = ifelse(biome2 %in% c('warm','oceanic', 'subtropical','tropical', 'montane'),1,0),
  borealer = ifelse(biome2 %in% c('boreal'),1,0)
)

cortab<-  cor(ecopts.montane.2[,c("tropicaler","warmer","borealer","Tg","m","s","Tw","Twh","Tc","Tclx",
                                  "d","p3AET","slope","hydric","sand","sealevel",
                                  "marine",'Tcc15','Tcc12','Tcc14','Tcc1215','Tcc913')],
              use = "pairwise.complete.obs") %>% as.data.frame()

rp <- rpart(biome2 ~ 
              Tcc1215+#Tcc15+Tcc12+Tcc14+Tcc913+
              Tg,
            data = ecopts.montane.2, control = list(maxdepth = 3, cp=0.005, minsplit=5))
# summary(rp)
png(filename="output/biome2.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp, extra=108) # Make plot
dev.off()


x=c(0,-15)
y=c(12, 0)
mod <- lm(y~x)
summary(mod)
x=c(-40:12)
y=(x*1+12)
plot(y~x)
#biome3 ----
biomeveg.e <- rast('output/global/biomeveg.e.tif')
rastbrick <- c(biomeveg.e, chm, Tw,Twh,Tg,Tc,Tclx,m,s,d,e,p3AET,slope, hydric, elev, sealevel,clay, sand,marine,soilpH,bedrock)
ecopts.biome.1 <- rastbrick %>% extract(ecopts.montane)
ecopts.biome.2 <- subset(ecopts.biome.1, #!vegcover %in% c(700,800,600) & Tg >= 12+
                           !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))
ecopts.biome.2 <- ecopts.biome.2 %>% group_by(vegcover) %>% mutate(ct=length(vegcover), wt=1/(ct+1))
ecopts.biome.2 <- ecopts.biome.2 %>% mutate(shrubby = ifelse(vegcover %in% c(310, 320), 1,0), d150 = d - pmin(s,050),s150 = s - pmin(d,150), 
                                            rd150 = (d - pmin(s,050))/(e+1),rs150 = (s - pmin(d,150))/(e+1),
                                            soilmin = ifelse(d > s,d*0,  pmax(d*0, (150-d)/150*100)),
                                            soilmax = ifelse(s > d,d*0+100,  pmin(d*0+100, s/150*100)),
                                            mtrans = m/(1+m), mtrans2 = m/(0.333+m), mlog = log(m+0.01))
cortab<-  cor(ecopts.biome.2[ecopts.biome.2$Tg >=12 & ecopts.biome.2$Tclx >= -15 & ecopts.biome.2$m >=0.5 & ecopts.biome.2$m < 1 ,c("shrubby","Tg","m","s","Tw","Twh","Tc","Tclx","d150","s150","rd150","rs150",
                                               "d","p3AET","slope","hydric","sand","sealevel","marine","soilmin","soilmax",
                                               "mtrans","mtrans2","mlog")], use = "pairwise.complete.obs") %>% as.data.frame()

ecopts.biome.2 <- ecopts.biome.2 %>% mutate(D150 = ifelse(d>=150,1,0),D200 = ifelse(d>=200,1,0),D250 = ifelse(d>=250,1,0),
                                            S150 = ifelse(s>=150,1,0),S200 = ifelse(s>=200,1,0),S250 = ifelse(s>=250,1,0),
                                            PAET150 = ifelse(p3AET>=150,1,0),PAET200 = ifelse(p3AET>=200,1,0),PAET250 = ifelse(p3AET>=250,1,0),
                                            M025 = ifelse(m >= .25, 1,0),M033 = ifelse(m >= 0.33, 1,0),M050 = ifelse(m >= 0.5, 1,0),
                                            M100 = ifelse(m >= 1, 1,0),M200 = ifelse(m >= 2, 1,0),
                                            Tc0Txclx_15 = ifelse(Tc >= 0 & Tclx >= -15, 1,0),Tc12Txclx0 = ifelse(Tc >= 12 & Tclx >= 0, 1,0),Tc15Txclx0 = ifelse(Tc >= 15 & Tclx >= 0, 1,0),
                                            Tg06 = ifelse(Tg >= 6, 1,0), Tg12 = ifelse(Tg >= 12, 1,0), Tg15 = ifelse(Tg >= 15, 1,0), Tg18 = ifelse(Tg >= 18, 1,0), Tg24 = ifelse(Tg >= 24, 1,0))
rp <- rpart(vegcover ~ 
              D150+D200+D250+S150+S150+S150+S200+S250+PAET150+PAET200+PAET250+
              M025+M033+M025+M050+M050+M100+M200+
              Tc0Txclx_15+Tc12Txclx0+Tc15Txclx0+
              Tg06+Tg12+Tg15+Tg18+Tg24+
              sand+bedrock+slope,
            data = ecopts.biome.2, method = 'class', weights= ecopts.biome.2$wt, control = list(maxdepth = 7, cp=0.01, minsplit=5))

rp <- rpart(vegcover ~
              d+m+s+p3AET+Tc+Tclx+Tg+#Tw+
              sand+bedrock+slope,
            data = ecopts.biome.2, method = 'class', weights= ecopts.biome.2$wt, control = list(maxdepth = 7, cp=0.01, minsplit=5))
# summary(rp)
png(filename="output/biome3.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp, extra=108) # Make plot
dev.off()
#tropic ----
ecopts.biome.2 <- subset(ecopts.biome.1, !vegcover %in% c(700,800,600) & Tc >= 12 & Tclx >= 0 & 
                           !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))
ecopts.biome.2 <- ecopts.biome.2 %>% group_by(vegcover) %>% mutate(ct=length(vegcover), wt=1/(ct+1))

rp <- rpart(vegcover ~ 
              d+m+s+p3AET+
              slope+sand,
            data = ecopts.biome.2, method = 'class', weights= ecopts.biome.2$wt, control = list(maxdepth = 5, cp=0.01, minsplit=5))
# summary(rp)
png(filename="output/biome3.trop.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp, extra=108) # Make plot
dev.off()

#warm ----
ecopts.biome.2 <- subset(ecopts.biome.1, !vegcover %in% c(700,800,600) & Tg >= 6 & Tc >= 0 & Tclx >= -15 & (Tclx < 0 | Tc < 12) &
                           !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))
ecopts.biome.2 <- ecopts.biome.2 %>% group_by(vegcover) %>% mutate(ct=length(vegcover), wt=1/(ct+1))

rp <- rpart(vegcover ~ 
              d+m+s+p3AET+
              slope+sand,
            data = ecopts.biome.2, method = 'class', weights= ecopts.biome.2$wt, control = list(maxdepth = 5, cp=0.01, minsplit=5))
# summary(rp)
png(filename="output/biome3.warm.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp, extra=108) # Make plot
dev.off()

#cold ----
ecopts.biome.2 <- subset(ecopts.biome.1, !vegcover %in% c(700,800,600) & Tg >= 12 & (Tc < 0 | Tclx < -15) & 
                           !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))
ecopts.biome.2 <- ecopts.biome.2 %>% group_by(vegcover) %>% mutate(ct=length(vegcover), wt=1/(ct+1))

rp <- rpart(vegcover ~ 
              d+m+s+p3AET+
              slope+sand,
            data = ecopts.biome.2, method = 'class', weights= ecopts.biome.2$wt, control = list(maxdepth = 5, cp=0.01, minsplit=5))
# summary(rp)
png(filename="output/biome3.cold.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp, extra=108) # Make plot
dev.off()
#boreal ----
ecopts.biome.2 <- subset(ecopts.biome.1, !vegcover %in% c(700,800) & Tg < 12 & (Tc < 0 | Tclx < -15) & 
                           !is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))
ecopts.biome.2 <- ecopts.biome.2 %>% group_by(vegcover) %>% mutate(ct=length(vegcover), wt=1/(ct+1))

rp <- rpart(vegcover ~ 
              d+m+s+p3AET+Tg+Tw+Tc+Tclx+
              slope+sand,
            data = ecopts.biome.2, method = 'class', weights= ecopts.biome.2$wt, control = list(maxdepth = 6, cp=0.01, minsplit=5))
# summary(rp)
png(filename="output/biome3.boreal.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp, extra=108) # Make plot
dev.off()
















ecopts3 <- ecopts3 %>% 
  mutate(Tcc = pmin(Tc, Tclx+15), s050 = s - pmin(d,050),s100 = s - pmin(d,100),s150 = s - pmin(d,150),s150 = s - pmin(d,150), 
         s200 = s - pmin(d,200), s250 = s - pmin(d,250),s300 = s - pmin(d,300),
         d050 = d - pmin(s,050),d100 = d - pmin(s,100),d150 = d - pmin(s,150),d150 = d - pmin(s,150), 
         d200 = d - pmin(s,200), d250 = d - pmin(s,250),d300 = d - pmin(s,300))


ecopts3 <- ecopts3 %>% group_by(BIOME_NAME) %>% mutate(ct = length(BIOME_NAME), wt = 1/(100+ct))

rp <- rpart(BIOME_NAME ~ 
              Tcc+Tclx+Tg+m+s+Tc+Tw+Twh+Tc+Tclx+
              d050+d100+d150+d200+d250+d300+
              s050+s100+s150+s200+s250+s300+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts3,  weights= ecopts3$wt, method="class",  control = list(maxdepth = 5, cp=0.001, minsplit=5))

png(filename="output/newbiome.png",width = 10, height = 3, units = 'in', res = 400)
rpart.plot(rp, extra=108) # Make plot
dev.off()
# biomas <- unique(subset(st_drop_geometry(eco), select=c('BIOME', 'BIOME_NAME')))
ecopts4 <- ecopts3 %>% subset(BIOME %in% c(1,2,3,4,5,6)) %>% 
  mutate(type = ifelse(BIOME %in% c(1,3,5,6),"Evergreen","Deciduous"))
ecopts4 <- ecopts4 %>% group_by(type) %>% mutate(ct = length(type), wt = 1/(100+ct))

rp <- rpart(type ~ 
              Tcc+Tclx+Tg+m+s+Tc+Tw+Twh+Tc+Tclx+
              d050+d100+d150+d200+d250+d300+
              s050+s100+s150+s200+s250+s300+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts4,  weights= ecopts4$wt, method="class",  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/newbiome.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp, extra=108) # Make plot
dev.off()

ecopts4 <- ecopts3 %>% subset(BIOME %in% c(1,2,3,4,5,7,8)) %>% 
  mutate(type = ifelse(BIOME %in% c(7,8),"Grassland","Forest"))
ecopts4 <- ecopts4 %>% group_by(type) %>% mutate(ct = length(type), wt = 1/(100+ct))

rp <- rpart(type ~ 
              Tcc+Tclx+Tg+m+s+Tc+Tw+Twh+Tc+Tclx+
              d050+d100+d150+d200+d250+d300+
              s050+s100+s150+s200+s250+s300+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts4,  weights= ecopts4$wt, method="class",  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/newbiome.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp, extra=108) # Make plot
dev.off()

ecopts4 <- ecopts3 %>% subset(BIOME %in% c(1,2,3,4,5,6,10,11)) %>% 
  mutate(type = ifelse(BIOME %in% c(10,11),"Alpine","Forest"))
ecopts4 <- ecopts4 %>% group_by(type) %>% mutate(ct = length(type), wt = 1/(100+ct))

rp <- rpart(type ~ 
              Tcc+Tclx+Tg+m+s+Tc+Tw+Twh+Tc+Tclx+
              d050+d100+d150+d200+d250+d300+
              s050+s100+s150+s200+s250+s300+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts4,  weights= ecopts4$wt, method="class",  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/newbiome.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp, extra=108) # Make plot
dev.off()

ecopts4 <- ecopts3 %>% 
  mutate(type = ifelse(BIOME %in% c(13),"Desert","NotDesert"))
ecopts4 <- ecopts4 %>% group_by(type) %>% mutate(ct = length(type), wt = 1/(100+ct))

rp <- rpart(type ~ 
              Tcc+Tclx+Tg+m+s+Tc+Tw+Twh+Tc+Tclx+
              d050+d100+d150+d200+d250+d300+
              s050+s100+s150+s200+s250+s300+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts4,  weights= ecopts4$wt, method="class",  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/newbiome.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp, extra=108) # Make plot
dev.off()


ecopts4 <- ecopts3 %>% 
  mutate(type = ifelse(BIOME %in% c(14),"Mangrove","NotMangrowe"))
ecopts4 <- ecopts4 %>% group_by(type) %>% mutate(ct = length(type), wt = 1/(100+ct))

rp <- rpart(type ~ 
              Tcc+Tclx+Tg+m+s+Tc+Tw+Twh+Tc+Tclx+
              d050+d100+d150+d200+d250+d300+
              s050+s100+s150+s200+s250+s300+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts4,  weights= ecopts4$wt, method="class",  control = list(maxdepth = 4, cp=0.001, minsplit=5))

png(filename="output/newbiome.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp, extra=108) # Make plot
dev.off()

ecopts4 <- ecopts3 %>% subset(BIOME %in% c(1,2,4)) %>% 
  mutate(type = ifelse(BIOME %in% c(1),"Evergreen","Deciduous"))
ecopts4 <- ecopts4 %>% group_by(type) %>% mutate(ct = length(type), wt = 1/(100+ct))

rp <- rpart(type ~ 
              Tcc+Tclx+Tg+m+s+Tc+Tw+Twh+Tc+Tclx+
              d050+d100+d150+d200+d250+d300+
              s050+s100+s150+s200+s250+s300+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts4,  weights= ecopts4$wt, method="class",  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/newbiome.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp, extra=108) # Make plot
dev.off()

ecopts4 <- ecopts3 %>% subset(BIOME %in% c(4,5,12)) %>% 
  mutate(type = ifelse(BIOME %in% c(12),"Scrub","Forest"))
ecopts4 <- ecopts4 %>% group_by(type) %>% mutate(ct = length(type), wt = 1/(100+ct))

rp <- rpart(type ~ 
              Tcc+Tclx+Tg+m+s+Tc+Tw+Twh+Tc+Tclx+
              d050+d100+d150+d200+d250+d300+
              s050+s100+s150+s200+s250+s300+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts4,  weights= ecopts4$wt, method="class",  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/newbiome.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp, extra=108) # Make plot
dev.off()

ecopts4 <- ecopts3 %>% subset(BIOME %in% c(8,12)) %>% 
  mutate(type = ifelse(BIOME %in% c(12),"Scrub","Grassland"))
ecopts4 <- ecopts4 %>% group_by(type) %>% mutate(ct = length(type), wt = 1/(100+ct))

rp <- rpart(type ~ 
              Tcc+Tclx+Tg+m+s+Tc+Tw+Twh+Tc+Tclx+
              d050+d100+d150+d200+d250+d300+
              s050+s100+s150+s200+s250+s300+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts4,  weights= ecopts4$wt, method="class",  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/newbiome.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp, extra=108) # Make plot
dev.off()

ecopts4 <- ecopts3 %>% subset(BIOME %in% c(12,13)) %>% 
  mutate(type = ifelse(BIOME %in% c(12),"Scrub","Desert"))
ecopts4 <- ecopts4 %>% group_by(type) %>% mutate(ct = length(type))

rp <- rpart(type ~ 
              Tcc+Tclx+Tg+m+s+Tc+Tw+Twh+Tc+Tclx+
              d050+d100+d150+d200+d250+d300+
              s050+s100+s150+s200+s250+s300+
              d+p3AET+slope+hydric+sand+sealevel+marine,
            data = ecopts4,  weights= ecopts4$wt, method="class",  control = list(maxdepth = 3, cp=0.001, minsplit=5))

png(filename="output/newbiome.png",width = 10, height = 3, units = 'in', res = 300)
rpart.plot(rp, extra=108) # Make plot
dev.off()


ecopts3 <- ecopts3 %>% group_by(BIOME) %>% mutate(ct = length(BIOME), wt = 1/(100+ct))
ecopts3 <- ecopts3 %>% mutate(wt2 = mean(wt), wt3 = wt+wt2)


rf <- ranger(as.factor(BIOME) ~ Tclx+Tg+m+s+Tc+Tw+Twh+Tc+Tclx+
               d+p3AET+slope+hydric+sand+clay+sealevel+marine+soilpH,
             data=ecopts3, num.trees=100, sample.fraction = 0.2, 
             max.depth = 25,  write.forest = TRUE)

output <- predict(object=rastbrick,  model=rf, na.rm=TRUE)
plot(output$predictions)
writeRaster(output, 'output/biomraster.tif', overwrite=T)


rf <- ranger(as.factor(altbiome2) ~ Tclx+Tg+m+s+Tc+Tw+Twh+Tc+Tclx+
               d+p3AET+slope+hydric+sand+clay+sealevel+marine+soilpH+bedrock,
             data=ecopts3, num.trees=100, sample.fraction = 0.2, 
             max.depth = 25,  write.forest = TRUE)

output <- predict(object=rastbrick,  model=rf, na.rm=TRUE)
plot(output$predictions)
writeRaster(output, 'output/altbiome.tif', overwrite=T)

ecosummary <- ecopts3 %>% group_by(ECO_ID) %>% 
  summarise(m = median(m), p3AET = median(p3AET), Tg = median(Tg), Tcc = median(Tcc))
ecosummary <- ecosummary %>% left_join(ecoalt)
write.csv(ecosummary, 'output/ecosummary2.csv', row.names = F, na = "")
library(raster)
library(sf)
library(fasterize)
library(sp)
library(ranger)
library(terra)
library(rgbif)
library(randomForest)
library(gdalUtilities)
library(stringr)
library(plyr)
library(Hmisc)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

path = 'nam5k/'
Tw <- rast(paste0(path, 'Tw.tif'))
#Resample SAGA topographic wetness index ----
# twi1km <- rast(paste0('data/twi1km.tif')) #SAGA topographic wetness index 1 km res.
# #twi5km <- resample(twi1km, Tw)
# #writeRaster(twi5km, 'data/twi5km.tif', overwrite=T)
# #twi5kmmin <- aggregate(twi1km, fact=5, fun="min", na.rm=T)
# #writeRaster(twi5kmmin, 'data/twi5kmmin.tif', overwrite=T)
# river <- aggregate(twi1km, fact=3, fun="max", na.rm=T); names(river) <- 'river'
# river <- project(river, Tw)
# writeRaster(river, paste0(path, 'river.tif'), overwrite=T)#SAGA topographic wetness index 5 km res.

systime = Sys.time()

# Vegetation cover ----
bps <- rast('data/BPS.tif')
#bps <- aggregate(bps, fact=5, fun='modal' )
bpsrat <- foreign::read.dbf('data/BPS.dbf')
bps.types <- unique(bpsrat[,c('BPS_NAME','GROUPNAME','GROUPVEG')])
write.csv(bps.types, 'data/bps.types.csv', row.names = F)
bps.types <- read.csv('data/bps.types.cover.csv')
bpsrat <- merge(bpsrat, bps.types, by=c('BPS_NAME','GROUPNAME','GROUPVEG'), all.x=T)
bps.df <- as.data.frame(bps, xy=T)
bps.df$VALUE_1 <- as.numeric(as.character(bps.df$BPS))
bps.df <- merge(bps.df, bpsrat, by='VALUE_1')
bps.wet <-  rast(cbind(x=bps.df$x,y=bps.df$y,bps.wet=bps.df$wet), type="xyz", crs=crs(bps))
bps.veg <-  rast(cbind(x=bps.df$x,y=bps.df$y,bps.veg=bps.df$vegcover), type="xyz", crs=crs(bps))
bps.wood <-  rast(cbind(x=bps.df$x,y=bps.df$y,bps.wood=bps.df$woodycover), type="xyz", crs=crs(bps))
bps.tree <-  rast(cbind(x=bps.df$x,y=bps.df$y,bps.tree=bps.df$treecover), type="xyz", crs=crs(bps))
bps.ever <-  rast(cbind(x=bps.df$x,y=bps.df$y,bps.ever=bps.df$evergreen), type="xyz", crs=crs(bps))
bps.wet <- aggregate(bps.wet, fact= 3, fun='mean', na.rm=T)
bps.veg <- aggregate(bps.veg, fact= 3, fun='mean', na.rm=T)
bps.wood <- aggregate(bps.wood, fact= 3, fun='mean', na.rm=T)
bps.tree <- aggregate(bps.tree, fact= 3, fun='mean', na.rm=T)
bps.ever <- aggregate(bps.ever, fact= 3, fun='mean', na.rm=T)
bps.wet <-  project(bps.wet, Tw)
bps.veg <-  project(bps.veg, Tw)
bps.wood <-  project(bps.wood, Tw)
bps.tree <-  project(bps.tree, Tw)
bps.ever <-  project(bps.ever, Tw)


kuchler <- sf::read_sf('data/kuchler_DD83.shp')
kuchler[kuchler$KUCHLER_ %in% 286,]$KUCHLER_ID <- 97 #fix map error
kuchler[kuchler$KUCHLER_ %in% 286,]$CODE <- 97 #fix map error
kuchler[kuchler$KUCHLER_ %in% 286,]$TYPE <- 'Northern hardwoods' #fix map error
kuchler[kuchler$KUCHLER_ %in% 1352,]$KUCHLER_ID <- 6 #fix map error
kuchler[kuchler$KUCHLER_ %in% 1352,]$CODE <- 6 #fix map error
kuchler[kuchler$KUCHLER_ %in% 1352,]$TYPE <- 'Redwood forest' #fix map error
kuchler.types <- unique(st_drop_geometry(kuchler[,c('TYPE','CODE')]))
write.csv(kuchler.types, 'data/kuchler.types.csv', row.names = F)
kuchler.types <-  read.csv('data/kuchler.types.cover.csv')
kuchler <- merge(kuchler, kuchler.types, by=c('TYPE','CODE'))
kuchler <- st_transform(kuchler, crs(Tw))
kuchler.wet <- rast(fasterize(kuchler, raster(Tw), field = 'wet'))
kuchler.veg <- rast(fasterize(kuchler, raster(Tw), field = 'vegcover'))
kuchler.wood <- rast(fasterize(kuchler, raster(Tw), field = 'woodycover'))
kuchler.tree <- rast(fasterize(kuchler, raster(Tw), field = 'treecover'))
kuchler.ever <- rast(fasterize(kuchler, raster(Tw), field = 'evergreen'))
names(kuchler.wet) <- 'kuchler.wet'
names(kuchler.veg) <- 'kuchler.veg'
names(kuchler.wood) <- 'kuchler.wood'
names(kuchler.tree) <- 'kuchler.tree'
names(kuchler.ever) <- 'kuchler.ever'
writeRaster(kuchler.tree, 'nam5k/kuchler.tree.tif', overwrite=T)

wwfeco <- sf::read_sf('data/ecoregions.shp')
wwfeco.types <- unique(st_drop_geometry(wwfeco[,c('ECO_ID','REALM','BIOME','BIOME_NAME','ECO_NAME')]))
write.csv(wwfeco.types, 'data/wwfeco.types.csv', row.names = F)
wwfeco.types <-  read.csv('data/wwfeco.types.cover.csv')
wwfeco<- merge(wwfeco, wwfeco.types, by=c('ECO_ID','REALM','BIOME','BIOME_NAME','ECO_NAME'))
wwfeco <- st_transform(wwfeco, crs(Tw))
wwfeco.wet <- rast(fasterize(wwfeco, raster(Tw), field = 'wet'))
wwfeco.veg <- rast(fasterize(wwfeco, raster(Tw), field = 'vegcover'))
wwfeco.wood <- rast(fasterize(wwfeco, raster(Tw), field = 'woodycover'))
wwfeco.tree <- rast(fasterize(wwfeco, raster(Tw), field = 'treecover'))
wwfeco.ever <- rast(fasterize(wwfeco, raster(Tw), field = 'evergreen'))
names(wwfeco.wet) <- 'wwfeco.wet'
names(wwfeco.veg) <- 'wwfeco.veg'
names(wwfeco.wood) <- 'wwfeco.wood'
names(wwfeco.tree) <- 'wwfeco.tree'
names(wwfeco.ever) <- 'wwfeco.ever'


brown <- sf::read_sf('data/biotic_comm_la.shp')
brown <- subset(brown, !BIOTIC12B_ %in% c(165,248,348,376,391,399,401,439,457,461))#omit unreasonable forest outliers north of Brooks Range
brown.types <- unique(st_drop_geometry(brown[,c('BIOMENAME1','biome')]))
write.csv(brown.types, 'data/brown.types.csv', row.names = F)
brown.types <-  read.csv('data/brown.types.cover.csv')
brown <- merge(brown, brown.types, by=c('BIOMENAME1','biome'))
brown <- st_transform(brown, crs(Tw))
brown.wet <- rast(fasterize(brown, raster(Tw), field = 'wet'))
brown.veg <- rast(fasterize(brown, raster(Tw), field = 'vegcover'))
brown.wood <- rast(fasterize(brown, raster(Tw), field = 'woodycover'))
brown.tree <- rast(fasterize(brown, raster(Tw), field = 'treecover'))
brown.ever <- rast(fasterize(brown, raster(Tw), field = 'evergreen'))
names(brown.wet) <- 'brown.wet'
names(brown.veg) <- 'brown.veg'
names(brown.wood) <- 'brown.wood'
names(brown.tree) <- 'brown.tree'
names(brown.ever) <- 'brown.ever'

wet.df <- c(brown.wet, wwfeco.wet, kuchler.wet, bps.wet)
wet.df <- as.data.frame(wet.df, xy=T, na.rm=F)
wet.df <- subset(wet.df, !is.na(brown.wet) |!is.na(wwfeco.wet) |!is.na(kuchler.wet) |!is.na(bps.wet))
wet.df$wet <- apply(wet.df[,3:6], MARGIN = 1, FUN = 'mean', na.rm=T)
wet.df$wet <- ifelse(!is.na(wet.df$kuchler.wet), (wet.df$kuchler.wet*3+wet.df$wet)/4,wet.df$wet)
wet.df$wet <- ifelse(!is.na(wet.df$bps.wet), (wet.df$bps.wet*3+wet.df$wet)/4,wet.df$wet)
wet <- rast(cbind(x=wet.df$x,y=wet.df$y,wet=wet.df$wet), type="xyz", crs=crs(Tw))
plot(wet)
writeRaster(wet,'nam5k/wet.tif', overwrite=T)
tree.df <- c(brown.tree, wwfeco.tree, kuchler.tree, bps.tree)
tree.df <- as.data.frame(tree.df, xy=T, na.rm=F)
tree.df <- subset(tree.df, !is.na(brown.tree) |!is.na(wwfeco.tree) |!is.na(kuchler.tree) |!is.na(bps.tree))
tree.df$tree <- apply(tree.df[,3:6], MARGIN = 1, FUN = 'mean', na.rm=T)
tree.df$tree <- ifelse(!is.na(tree.df$kuchler.tree), (tree.df$kuchler.tree*2+tree.df$tree)/3,tree.df$tree)
tree.df$tree <- ifelse(!is.na(tree.df$bps.tree), (tree.df$bps.tree*2+tree.df$tree)/3,tree.df$tree)
tree <- rast(cbind(x=tree.df$x,y=tree.df$y,tree=tree.df$tree), type="xyz", crs=crs(Tw))
plot(tree)
writeRaster(tree,'nam5k/tree.tif', overwrite=T)
wood.df <- c(brown.wood, wwfeco.wood, kuchler.wood, bps.wood)
wood.df <- as.data.frame(wood.df, xy=T, na.rm=F)
wood.df <- subset(wood.df, !is.na(brown.wood) |!is.na(wwfeco.wood) |!is.na(kuchler.wood) |!is.na(bps.wood))
wood.df$wood <- apply(wood.df[,3:6], MARGIN = 1, FUN = 'mean', na.rm=T)
wood.df$wood <- ifelse(!is.na(wood.df$kuchler.wood), (wood.df$kuchler.wood*2+wood.df$wood)/3,wood.df$wood)
wood.df$wood <- ifelse(!is.na(wood.df$bps.wood), (wood.df$bps.wood*2+wood.df$wood)/3,wood.df$wood)
wood <- rast(cbind(x=wood.df$x,y=wood.df$y,wood=wood.df$wood), type="xyz", crs=crs(Tw))
plot(wood)
writeRaster(wood,'nam5k/wood.tif', overwrite=T)
veg.df <- c(brown.veg, wwfeco.veg, kuchler.veg, bps.veg)
veg.df <- as.data.frame(veg.df, xy=T, na.rm=F)
veg.df <- subset(veg.df, !is.na(brown.veg) |!is.na(wwfeco.veg) |!is.na(kuchler.veg) |!is.na(bps.veg))
veg.df$veg <- apply(veg.df[,3:6], MARGIN = 1, FUN = 'mean', na.rm=T)
veg.df$veg <- ifelse(!is.na(veg.df$kuchler.veg), (veg.df$kuchler.veg*2+veg.df$veg)/3,veg.df$veg)
veg.df$veg <- ifelse(!is.na(veg.df$bps.veg), (veg.df$bps.veg*2+veg.df$veg)/3,veg.df$veg)
veg <- rast(cbind(x=veg.df$x,y=veg.df$y,veg=veg.df$veg), type="xyz", crs=crs(Tw))
plot(veg)
writeRaster(veg,'nam5k/veg.tif', overwrite=T)
ever.df <- c(brown.ever, wwfeco.ever, kuchler.ever, bps.ever)
ever.df <- as.data.frame(ever.df, xy=T, na.rm=F)
ever.df <- subset(ever.df, !is.na(brown.ever) |!is.na(wwfeco.ever) |!is.na(kuchler.ever) |!is.na(bps.ever))
ever.df$ever <- apply(ever.df[,3:6], MARGIN = 1, FUN = 'mean', na.rm=T)
ever.df$ever <- ifelse(!is.na(ever.df$kuchler.ever), (ever.df$kuchler.ever*2+ever.df$ever)/3,ever.df$ever)
ever.df$ever <- ifelse(!is.na(ever.df$bps.ever), (ever.df$bps.ever*2+ever.df$ever)/3,ever.df$ever)
ever <- rast(cbind(x=ever.df$x,y=ever.df$y,ever=ever.df$ever), type="xyz", crs=crs(Tw))
plot(ever)
writeRaster(ever,'nam5k/ever.tif', overwrite=T)
# bps2 <- project(bps, twi1km, method='near')
# bps <- aggregate(bps2, fact=5, )

#Load rasters ----
path = 'nam5k/'
Tw <- rast(paste0(path, 'Tw.tif')); names(Tw)<-'Tw'
Twh <- rast(paste0(path, 'Twh.tif')); names(Twh)<-'Twh'
Tclx <- rast(paste0(path, 'Tclx.tif')); names(Tclx)<-'Tclx'
Tg <- rast(paste0(path, 'Tg.tif')); names(Tg)<-'Tg'
Tc <- rast(paste0(path, 'Tc.tif')); names(Tc)<-'Tc'
m <- rast(paste0(path, 'm.tif')); names(m)<-'m'
p <- rast(paste0(path, 'p.tif')); names(p)<-'p'
d <- rast(paste0(path, 'd.tif')); names(d)<-'d'
s <- rast(paste0(path, 's.tif')); names(s)<-'s'
pAET <- rast(paste0(path, 'pAET.tif')); names(pAET)<-'pAET'
p3AET <- rast(paste0(path, 'p3AET.tif')); names(p3AET)<-'p3AET'
m <- m/(1+m)*100
s <- s/(s+25)*100
d <- d/(d+150)*100
p3AET <- p3AET/(p3AET+75)*100
slope <- rast(paste0(path, 'slope.tif')); names(slope)<-'slope'
sand <- rast(paste0(path, 'sand.tif')); names(sand)<-'sand'
SoilpH <- rast(paste0(path, 'SoilpH.tif')); names(SoilpH)<-'SoilpH'
hydric <- rast(paste0(path, 'hydric.tif')); names(hydric)<-'hydric'
salids <- rast(paste0(path, 'salids.tif')); names(salids)<-'salids'
water100k <- rast(paste0(path, 'water100k.tif')); names(water100k)<-'water100k'
sealevel <- rast(paste0(path, 'sealevel.tif')); names(sealevel)<-'sealevel'
shore <- rast(paste0(path, 'shore.tif')); names(shore)<-'shore'
elev <- rast(paste0(path, 'elev.tif')); names(elev)<-'elev'
bedrock <- rast(paste0(path, 'bedrock.tif')); names(bedrock)<-'bedrock'
clay <- rast(paste0(path, 'clay.tif')); names(clay)<-'clay'
river <- rast(paste0(path, 'river.tif')); names(river)<-'river'
wet<-rast(paste0(path, 'wet.tif')); wet <- extend(wet, Tw)
veg<-rast(paste0(path, 'veg.tif')); veg <- extend(veg, Tw)
wood<-rast(paste0(path, 'wood.tif')); wood <- extend(wood, Tw)
tree<-rast(paste0(path, 'tree.tif')); tree <- extend(tree, Tw)
kuchler.tree<-rast(paste0(path, 'kuchler.tree.tif')); kuchler.tree <- extend(kuchler.tree, Tw)
shadw0 <- rast(paste0(path,'shadw0.tif'))
shadw45 <- rast(paste0(path,'shadw45.tif'))
shadw90 <- rast(paste0(path,'shadw90.tif'))
shadw135 <- rast(paste0(path,'shadw135.tif'))
shadw180 <- rast(paste0(path,'shadw180.tif'))
shadw225 <- rast(paste0(path,'shadw225.tif'))
shadw270 <- rast(paste0(path,'shadw270.tif'))
shadw315 <- rast(paste0(path,'shadw315.tif'))
#shadw0+shadw45+shadw90+shadw135+shadw180+shadw225+shadw270+shadw315

#wet model ---- 
rasters<-c(kuchler.tree, wet,veg,wood,tree,Tw,Twh,Tg,Tc,Tclx,m,s,d,p3AET,slope,
           sand,SoilpH,hydric,salids,shore,sealevel,elev,bedrock,clay,river)
r.df <- as.data.frame(rasters, xy=TRUE, na.rm=F)
r.df$tree <- ifelse(r.df$Tg < 4.5 | r.df$elev > 4000, 0, r.df$tree)
r.df$wood <- ifelse(r.df$Tw <= 1, 0, r.df$wood); r.df$veg <- ifelse(r.df$Tw <= 1, 0, r.df$veg)
r.df <-  subset(r.df,!is.na(sealevel) & !is.na(salids) & !is.na(Tg) & !is.na(Tc) & !is.na(river) &!is.na(elev) & !is.na(slope)  & !is.na(SoilpH) & !is.na(m) & !is.na(sand) & !is.na(clay))
r.df$wt <- ifelse(is.na(r.df$kuchler.tree), 1,1000)



r.df.s <- subset(r.df, !is.na(wet))
#Tw+Twh+Tg+Tc+Tclx+m+s+d+p3AET+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river
rf <- ranger(wet ~ 
               m+s+d+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river
             ,
             data=r.df.s, num.trees=25, max.depth = 12, importance = 'impurity', write.forest = TRUE, case.weights = r.df.s$wt)

r.df$output <- predictions(predict(rf, data=r.df))


wetmodel <- rast(cbind(x=r.df$x,y=r.df$y,z=r.df$output), type="xyz", crs=crs(Tw))
plot(wetmodel)
names(wetmodel) <- 'wetmodel'
wetmodel <- extend(wetmodel, Tw); wetmodel <- crop(wetmodel, Tw)

writeRaster(wetmodel, 'output/wetmodel.tif', overwrite=T)
#veg model ---- 
rasters<-c(kuchler.tree, wet,veg,wood,tree,Tw,Twh,Tg,Tc,Tclx,m,s,d,p3AET,slope,
           sand,SoilpH,hydric,salids,shore,sealevel,elev,bedrock,clay,river,wetmodel)
r.df <- as.data.frame(rasters, xy=TRUE, na.rm=F)
r.df$tree <- ifelse(r.df$Tg < 4.5 | r.df$elev > 4000, 0, r.df$tree)
r.df$wood <- ifelse(r.df$Tw <= 1, 0, r.df$wood); r.df$veg <- ifelse(r.df$Tw <= 1, 0, r.df$veg)
r.df <-  subset(r.df,!is.na(wetmodel)&!is.na(sealevel) & !is.na(salids) & !is.na(Tg) & !is.na(Tc) & !is.na(river) &!is.na(elev) & !is.na(slope)  & !is.na(SoilpH) & !is.na(m) & !is.na(sand) & !is.na(clay))
r.df$wt <- ifelse(is.na(r.df$kuchler.tree), 1,100)



r.df.s <- subset(r.df, !is.na(veg))
#Tw+Twh+Tg+Tc+Tclx+m+s+d+p3AET+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river
rf <- ranger(veg ~ 
               Tw+Twh+Tg+Tc+Tclx+m+s+d+p3AET+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river+wetmodel
             ,
             data=r.df.s, num.trees=25, max.depth = 15, importance = 'impurity', write.forest = TRUE, case.weights = r.df.s$wt)

r.df$output <- predictions(predict(rf, data=r.df))


vegmodel <- rast(cbind(x=r.df$x,y=r.df$y,z=r.df$output), type="xyz", crs=crs(Tw))
plot(vegmodel)
names(vegmodel) <- 'vegmodel'
writeRaster(vegmodel, 'output/vegmodel.tif', overwrite=T)
#wood model ---- 
rasters<-c(kuchler.tree, wet,veg,wood,tree,Tw,Twh,Tg,Tc,Tclx,m,s,d,p3AET,slope,
           sand,SoilpH,hydric,salids,shore,sealevel,elev,bedrock,clay,river,wetmodel)
r.df <- as.data.frame(rasters, xy=TRUE, na.rm=F)
r.df$tree <- ifelse(r.df$Tg < 4.5 | r.df$elev > 4000, 0, r.df$tree)
r.df$wood <- ifelse(r.df$Tw <= 1, 0, r.df$wood); r.df$veg <- ifelse(r.df$Tw <= 1, 0, r.df$veg)
r.df <-  subset(r.df,!is.na(wetmodel)&!is.na(sealevel) & !is.na(salids) & !is.na(Tg) & !is.na(Tc) & !is.na(river) &!is.na(elev) & !is.na(slope)  & !is.na(SoilpH) & !is.na(m) & !is.na(sand) & !is.na(clay))
r.df$wt <- ifelse(is.na(r.df$kuchler.tree), 1,100)



r.df.s <- subset(r.df, !is.na(wood))
#Tw+Twh+Tg+Tc+Tclx+m+s+d+p3AET+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river
rf <- ranger(wood ~ 
               Tw+Twh+Tg+Tc+Tclx+m+s+d+p3AET+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river+wetmodel
             ,
             data=r.df.s, num.trees=25, max.depth = 15, importance = 'impurity', write.forest = TRUE, case.weights = r.df.s$wt)

r.df$output <- predictions(predict(rf, data=r.df))


woodmodel <- rast(cbind(x=r.df$x,y=r.df$y,z=r.df$output), type="xyz", crs=crs(Tw))
plot(woodmodel)
names(woodmodel) <- 'woodmodel'
writeRaster(woodmodel, 'output/woodmodel.tif', overwrite=T)

#tree model ---- 
treenull <- aggregate(tree, fact=5, fun='max', na.rm=T)

treenull[treenull <=0] <- NA
plot(treenull)
treedist <- distance(treenull)

treedist <- resample(treedist, Tw)
treedist[treedist <=25000] <- 0;treedist[treedist >0] <- 1;treedist <- treedist*-1+1
treedist <- focal(treedist, w=focalMat(treedist, 25000, "circle"), fun="mean")


plot(treedist)
names(treedist) <- 'treedist'


rasters<-c(kuchler.tree, wet,veg,wood,tree,Tw,Twh,Tg,Tc,Tclx,m,s,d,p3AET,slope,
           sand,SoilpH,hydric,salids,shore,sealevel,elev,bedrock,clay,river,wetmodel)
r.df <- as.data.frame(rasters, xy=TRUE, na.rm=F)
r.df$tree <- ifelse(r.df$Tg < 4.5 | r.df$elev > 4000, 0, r.df$tree)
r.df$wood <- ifelse(r.df$Tw <= 1, 0, r.df$wood); r.df$veg <- ifelse(r.df$Tw <= 1, 0, r.df$veg)
r.df <-  subset(r.df,!is.na(wetmodel)&!is.na(sealevel) & !is.na(salids) & !is.na(Tg) & !is.na(Tc) & !is.na(river) &!is.na(elev) & !is.na(slope)  & !is.na(SoilpH) & !is.na(m) & !is.na(sand) & !is.na(clay))
r.df$wt <- ifelse(is.na(r.df$kuchler.tree), 1,100)
mean(r.df.s[r.df.s$Tg<4,]$tree)


r.df.s <- subset(r.df, !is.na(tree))#& !is.na(kuchler.tree))
#Tw+Twh+Tg+Tc+Tclx+m+s+d+p3AET+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river
rf <- ranger(tree ~ Tw+Twh+Tg+Tc+Tclx+m+s+d+p3AET+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river+wetmodel
             #+shadw0+shadw45+shadw90+shadw135+shadw180+shadw225+shadw270+shadw315
             #m+s+d+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river
             ,
             data=r.df.s, num.trees=25, max.depth = 15, importance = 'impurity', write.forest = TRUE, case.weights = r.df.s$wt)

r.df$output <- predictions(predict(rf, data=r.df))


pred.raster <- rast(cbind(x=r.df$x,y=r.df$y,z=r.df$output), type="xyz", crs=crs(Tw))
treedist <- extend(treedist, pred.raster)
treedist <- crop(treedist, pred.raster)
treemodel <- pred.raster*treedist
plot(treemodel)
names(treemodel) <- 'treemodel'
writeRaster(treemodel, 'output/treemodel.tif', overwrite=T)

#evergreen model ---- 
ever <- extend(ever, Tw); ever <- crop(ever, Tw)
vegmodel <- extend(vegmodel, Tw); vegmodel <- crop(vegmodel, Tw)
woodmodel <- extend(woodmodel, Tw); woodmodel <- crop(woodmodel, Tw)
treemodel <- extend(treemodel, Tw); treemodel <- crop(treemodel, Tw)
rasters<-c(kuchler.tree, ever, Tw,Twh,Tg,Tc,Tclx,m,s,d,p3AET,slope,
           sand,SoilpH,hydric,salids,shore,sealevel,elev,bedrock,clay,river,
           wetmodel,vegmodel,woodmodel,treemodel
        )
r.df <- as.data.frame(rasters, xy=TRUE, na.rm=F)
r.df$tree <- ifelse(r.df$Tg < 4.5 | r.df$elev > 4000, 0, r.df$tree)
r.df$wood <- ifelse(r.df$Tw <= 1, 0, r.df$wood); r.df$veg <- ifelse(r.df$Tw <= 1, 0, r.df$veg)
r.df <-  subset(r.df,!is.na(wetmodel)&!is.na(treemodel)&!is.na(sealevel) & !is.na(salids) & !is.na(Tg) & !is.na(Tc) & !is.na(river) &!is.na(elev) & !is.na(slope)  & !is.na(SoilpH) & !is.na(m) & !is.na(sand) & !is.na(clay))
r.df$wt <- ifelse(is.na(r.df$kuchler.tree), 1,100)



r.df.s <- subset(r.df, !is.na(ever))
#Tw+Twh+Tg+Tc+Tclx+m+s+d+p3AET+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river
rf <- ranger(ever ~ 
               Tw+Twh+Tg+Tc+Tclx+m+s+d+p3AET+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river
             +wetmodel+vegmodel+woodmodel+treemodel
             ,
             data=r.df.s, num.trees=25, max.depth = 15, importance = 'impurity', write.forest = TRUE, case.weights = r.df.s$wt)

r.df$output <- predictions(predict(rf, data=r.df))


evermodel <- rast(cbind(x=r.df$x,y=r.df$y,z=r.df$output), type="xyz", crs=crs(Tw))
plot(evermodel)
names(evermodel) <- 'evermodel'
writeRaster(evermodel, 'output/evermodel.tif', overwrite=T)

#derivatives ----

woodmodel <- min(woodmodel, vegmodel)
treemodel <- min(treemodel, woodmodel)
writeRaster(woodmodel, 'output/woodmodel.tif', overwrite=T)
writeRaster(treemodel, 'output/treemodel.tif', overwrite=T)
shrubby <- woodmodel-treemodel
grassy <- vegmodel - woodmodel
openveg <- shrubby+grassy
writeRaster(shrubby, 'output/shrubby.tif', overwrite=T)
writeRaster(grassy, 'output/grassy.tif', overwrite=T)
writeRaster(openveg, 'output/openveg.tif', overwrite=T)
Sys.time() - systime

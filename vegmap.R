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

##landcover ----
# globalcover <- rast('C:/a/geo/GlcShare_v10_Dominant/glc_shv10_DOM.Tif')
# elev <- rast('C:/workspace2/processclimategrids/wc2.1_2.5m_elev/wc2.1_2.5m_elev.tif')
# crs(globalcover) <- crs(elev)
# plot(globalcover)
# writeRaster(globalcover, 'data/globalcover.tif')
# x <- rast(extent = ext(Tw), res=c(900,900), crs=crs(Tw))
# nalandcover <- project(globalcover, x, method='near')
# writeRaster(nalandcover, 'data/nalandcover.tif')
# 
# landforest <- nalandcover
# landforest[landforest %in% c(0,1,2,11,6,7)] <- NA
# landforest[landforest %in% c(3,5,8,9,10)] <- -1
# landforest[landforest %in% 4] <- 0
# landforest <- landforest+1
# landforest1 <- aggregate(landforest, fact=3, fun='mean', na.rm=T)
# landforest1 <- project(landforest1, Tw)
# names(landforest1) <- 'land.forest'
# writeRaster(landforest1, 'data/land.forest.tif', overwrite=T)
# 
# landwood <- nalandcover
# landwood[landwood %in% c(0,1,2,11,6,7)] <- NA
# landwood[landwood %in% c(3,8,9,10)] <- -1
# landwood[landwood %in% c(4,5)] <- 0
# landwood <- landwood+1
# landwood1 <- aggregate(landwood, fact=3, fun='mean', na.rm=T)
# landwood1 <- project(landwood1, Tw)
# names(landwood1) <- 'land.wood'
# writeRaster(landwood1, 'data/land.wood.tif', overwrite=T)
# 
# landveg <- nalandcover
# landveg[landveg %in% c(0,1,2,11,6,7)] <- NA
# landveg[landveg %in% c(9,10)] <- -4
# landveg[landveg %in% c(3,4,5)] <- 0
# landveg[landveg %in% c(8)] <- -3
# landveg <- (landveg+4)/4
# landveg1 <- aggregate(landveg, fact=3, fun='mean', na.rm=T)
# landveg1 <- project(landveg1, Tw)
# names(landveg1) <- 'land.veg'
# writeRaster(landveg1, 'data/land.veg.tif', overwrite=T)
# 
# landwet <- nalandcover
# landwet[landwet %in% c(0,1)] <- NA
# landwet[landwet %in% c(2,3,4,5,8,9,10,11)] <- -1
# landwet[landwet %in% c(6,7)] <- 0
# landwet <- landwet+1
# landwet1 <- aggregate(landwet, fact=3, fun='mean', na.rm=T)
# landwet1 <- project(landwet1, Tw)
# names(landwet1) <- 'land.wet'
# writeRaster(landwet1, 'data/land.wet.tif', overwrite=T)
# 
# landhuman <- nalandcover
# landhuman[landhuman %in% c(0,11)] <- NA
# landhuman[landhuman %in% c(3,4,5,6,7,8,9,10)] <- -1
# landhuman[landhuman %in% c(1,2)] <- 0
# landhuman <- landhuman+1
# landhuman1 <- aggregate(landhuman, fact=3, fun='mean', na.rm=T)
# landhuman1 <- project(landhuman1, Tw)
# names(landhuman1) <- 'land.human'
# writeRaster(landhuman1, 'data/land.human.tif', overwrite=T)
land.wet <- rast('data/land.wet.tif')
land.veg <- rast('data/land.veg.tif')
land.wood <- rast('data/land.wood.tif')
land.forest <- rast('data/land.forest.tif')

michigan <- sf::read_sf('data/michiganohio/MichLP_1800veg.shp')
michiganlist <- unique(st_drop_geometry(michigan[,c('VEGCODE', 'LEGCODE', 'COVERTYPE')]))
write.csv(michiganlist, 'data/michiganohio/michiganlist.csv', row.names = F)
ohio <- sf::read_sf('data/michiganohio/vege_orig_a_OH.shp')
ohiolist <- unique(st_drop_geometry(ohio[,c('VEG_CDE', 'NAME')]))
write.csv(ohiolist, 'data/michiganohio/ohiolist.csv', row.names = F)
michiganlist <- read.csv('data/michiganohio/michiganlist.cover.csv')
ohiolist <- read.csv('data/michiganohio/ohiolist.cover.csv')
michigan <- merge(michigan, michiganlist, by=c('VEGCODE', 'LEGCODE', 'COVERTYPE'))
ohio <- merge(ohio, ohiolist, by=c('VEG_CDE', 'NAME'))
michigan <- st_transform(michigan, crs(Tw))
ohio <- st_transform(ohio, crs(Tw))
x <- rast(extent=ext(michigan), crs=crs(Tw), res=c(500,500))
michigan.wet <- rast(fasterize(michigan, raster(x), field = 'wet'))
michigan.veg <- rast(fasterize(michigan, raster(x), field = 'vegcover'))
michigan.wood <- rast(fasterize(michigan, raster(x), field = 'woodycover'))
michigan.tree <- rast(fasterize(michigan, raster(x), field = 'treecover'))
michigan.ever <- rast(fasterize(michigan, raster(x), field = 'evergreen'))

michigan.wet <- aggregate(michigan.wet, fact=5, fun='mean'); michigan.wet <-  project(michigan.wet, Tw)
michigan.veg <- aggregate(michigan.veg, fact=5, fun='mean'); michigan.veg <-  project(michigan.veg, Tw)
michigan.wood <- aggregate(michigan.wood, fact=5, fun='mean'); michigan.wood <-  project(michigan.wood, Tw)
michigan.tree <- aggregate(michigan.tree, fact=5, fun='mean'); michigan.tree <-  project(michigan.tree, Tw)
michigan.ever <- aggregate(michigan.ever, fact=5, fun='mean'); michigan.ever <-  project(michigan.ever, Tw)

x <- rast(extent=ext(ohio), crs=crs(Tw), res=c(500,500))
ohio.wet <- rast(fasterize(ohio, raster(x), field = 'wet'))
ohio.veg <- rast(fasterize(ohio, raster(x), field = 'vegcover'))
ohio.wood <- rast(fasterize(ohio, raster(x), field = 'woodycover'))
ohio.tree <- rast(fasterize(ohio, raster(x), field = 'treecover'))
ohio.ever <- rast(fasterize(ohio, raster(x), field = 'evergreen'))

ohio.wet <- aggregate(ohio.wet, fact=5, fun='mean'); ohio.wet <-  project(ohio.wet, Tw)
ohio.wood <- aggregate(ohio.wood, fact=5, fun='mean'); ohio.wood <-  project(ohio.wood, Tw)
ohio.veg <- aggregate(ohio.veg, fact=5, fun='mean'); ohio.veg <-  project(ohio.veg, Tw)
ohio.tree <- aggregate(ohio.tree, fact=5, fun='mean'); ohio.tree <-  project(ohio.tree, Tw)
ohio.ever <- aggregate(ohio.ever, fact=5, fun='mean'); ohio.ever <-  project(ohio.ever, Tw)


#Kuchler ----
kuchler <- sf::read_sf('data/kuchler_DD83.shp')
kuchler[kuchler$KUCHLER_ %in% 1040,]$KUCHLER_ID <- 88 #fix map error
kuchler[kuchler$KUCHLER_ %in% 1040,]$CODE <- 88 #fix map error
kuchler[kuchler$KUCHLER_ %in% 1040,]$TYPE <- 'Southeastn spruce-fir forest' #fix map error
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


#TN Barrens ----
tn1 <- read_sf('data/southerngrass/Grasslands_MidSouth_shapefiles/Grasslands_MidSouth_polys.shp')
tn2 <- read_sf('data/southerngrass/SGI_Southern_Blue_Ridge_balds/SGI_Southern_Blue_Ridge_balds.shp')
tn3 <- read_sf('data/southerngrass/OKNP_Central_Interior_Off_v2/OKNP_Central_Interior_Off_v2.shp')
tn4 <- read_sf('data/southerngrass/OKNP_Pennyroyal_Karst_Off_v2/OKNP_Pennyroyal_Karst_Off_v2.shp')
st_crs(tn4) <- st_crs(tn3)

tn1 <- st_transform(tn1, st_crs(Tw))
tn2 <- st_transform(tn2, st_crs(Tw))
tn3 <- st_transform(tn3, st_crs(Tw))
tn4 <- st_transform(tn4, st_crs(Tw))

xmin=min(c(ext(tn1)[1],ext(tn2)[1],ext(tn3)[1],ext(tn3)[1]))
xmax=max(c(ext(tn1)[2],ext(tn2)[2],ext(tn3)[2],ext(tn3)[2]))
ymin=min(c(ext(tn1)[3],ext(tn2)[3],ext(tn3)[3],ext(tn3)[3]))
ymax=max(c(ext(tn1)[4],ext(tn2)[4],ext(tn3)[4],ext(tn3)[4]))
grid.ncol = floor((xmax-xmin)/500+1)
grid.nrow = floor((ymax-ymin)/500+1)
unique(tn1$Grassland_)
x <- rast(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,crs=crs(tn1), ncol=grid.ncol, nrow=grid.nrow)
tn1$woodcover <- ifelse(tn1$Grassland_ %in% 'Prairies', 0,
                        ifelse(tn1$Grassland_ %in% 'Savannas', 25,60))
tn1$treecover <- ifelse(tn1$Grassland_ %in% 'Prairies', 0,
                        ifelse(tn1$Grassland_ %in% 'Savannas', 25,25))
tn1.wood <- rast(fasterize(tn1, raster(x), field = 'woodcover', fun = "min"))
tn1.tree <- rast(fasterize(tn1, raster(x), field = 'treecover', fun = "min"))
tn2$woodcover <- 25;tn3$woodcover <- 25;tn4$woodcover <- 25
tn2$treecover <- 0;tn3$treecover <- 10;tn4$treecover <- 10
tnx <- rbind(tn2[,c('woodcover','treecover','geometry')],
             tn3[,c('woodcover','treecover','geometry')],
             tn4[,c('woodcover','treecover','geometry')])
tn2.wood <- rast(fasterize(tnx, raster(x), field = 'woodcover', fun = "min"))
tn2.tree <- rast(fasterize(tnx, raster(x), field = 'treecover', fun = "min"))
tnx.wood <- min(tn1.wood, tn2.wood, na.rm=T)
tnx.tree <- min(tn1.tree, tn2.tree, na.rm=T)
tnx.wood[is.na(tnx.wood)]=95;tnx.tree[is.na(tnx.tree)]=95
tn.wood <- project(tnx.wood, Tw); tn.tree <- project(tnx.tree, Tw)


#compile ----
wet <- mean(brown.wet, wwfeco.wet, na.rm=T)
wet <- mean(wet, kuchler.wet, na.rm=T)
wet <- mean(wet,bps.wet,bps.wet,bps.wet,na.rm=T)
wet <- mean(wet,michigan.wet,ohio.wet,na.rm=T)
wet <- mean(wet,land.wet,na.rm=T)
names(wet) <- 'wet';writeRaster(wet, 'data/training/wet.tif', overwrite=T)
wet <- rast('data/training/wet.tif')
plot(wet)

veg <- mean(brown.veg, wwfeco.veg, na.rm=T)
veg <- mean(veg, kuchler.veg, na.rm=T)
veg <- mean(veg, bps.veg,bps.veg,bps.veg,  na.rm=T)
veg <- mean(veg, michigan.veg,michigan.veg,michigan.veg,ohio.veg,ohio.veg,  na.rm=T)
Tg <- rast(paste0(path, 'Tg.tif')); Tg.5 <- (Tg >= 5)*veg
land.veg.boreal <- veg*land.veg
veg <- max(land.veg.boreal,Tg.5, na.rm = T)
names(veg) <- 'veg';writeRaster(veg, 'data/training/veg.tif', overwrite=T)
veg <- rast('data/training/veg.tif')
plot(veg)

wood <- mean(brown.wood, wwfeco.wood, na.rm=T)
wood <- mean(wood, kuchler.wood, na.rm=T)
wood <- mean(wood, bps.wood,bps.wood,bps.wood,  na.rm=T)
wood <- mean(wood, michigan.wood,michigan.wood,michigan.wood,ohio.wood,ohio.wood,  na.rm=T)
wood <- min(wood, tn.wood,  na.rm=T)
# wet.herb <- focal(land.wet, fun='min')
# wet.herb <- (wet.herb*-0.75+1)*100
# wood <- min(tree, wet.herb,  na.rm=T)
Tg <- rast(paste0(path, 'Tg.tif')); Tg.7 <- (Tg >= 7)*wood
land.wood.boreal <- wood*land.wood
wood <- max(land.wood.boreal,Tg.7, na.rm = T)
names(wood) <- 'wood';writeRaster(wood, 'data/training/wood.tif', overwrite=T)
wood <- rast('data/training/wood.tif')



tree <- mean(brown.tree, wwfeco.tree, na.rm=T)
tree <- mean(tree, kuchler.tree, na.rm=T)
tree <- mean(tree, bps.tree,bps.tree,bps.tree,  na.rm=T)
tree <- mean(tree, michigan.tree,michigan.tree,michigan.tree,ohio.tree,ohio.tree,  na.rm=T)
tree <- min(tree, tn.tree,  na.rm=T)
# tree <- min(tree, wet.herb,  na.rm=T)

Tg <- rast(paste0(path, 'Tg.tif')); Tg.7 <- (Tg >= 7)*tree
land.forest.boreal <- tree*land.forest
tree <- max(land.forest.boreal,Tg.7, na.rm = T)
names(tree) <- 'tree'
tree <- rast('data/training/tree.tif')
plot(tree)


ever <- mean(brown.ever, wwfeco.ever, na.rm=T)
ever <- mean(ever, kuchler.ever, na.rm=T)
ever <- mean(ever, bps.ever,bps.ever,bps.ever,  na.rm=T)
ever <- mean(ever, michigan.ever,michigan.ever,michigan.ever,ohio.ever,ohio.ever,  na.rm=T)
names(ever) <- 'ever';writeRaster(ever, 'data/training/ever.tif', overwrite=T)
ever <- rast('data/training/ever.tif')
plot(ever)


#Load rasters ----
path = 'nam5k/'
Tw <- rast(paste0(path, 'Tw.tif')); names(Tw)<-'Tw'
Twh <- rast(paste0(path, 'Twh.tif')); names(Twh)<-'Twh'
Tclx <- rast(paste0(path, 'Tclx.tif')); names(Tclx)<-'Tclx'
Tcl <- rast(paste0(path, 'Tcl.tif')); names(Tcl)<-'Tcl'
Tg <- rast(paste0(path, 'Tg.tif')); names(Tg)<-'Tg'
Tc <- rast(paste0(path, 'Tc.tif')); names(Tc)<-'Tc'
a <- rast(paste0(path, 'a.tif')); names(a)<-'a'
e <- rast(paste0(path, 'e.tif')); names(e)<-'e'
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
wet <- rast('data/training/wet.tif')
veg <- rast('data/training/veg.tif')
wood <- rast('data/training/wood.tif')
tree <- rast('data/training/tree.tif')
ever <- rast('data/training/ever.tif')

kuchler.tree<-rast(paste0(path, 'kuchler.tree.tif')); kuchler.tree <- extend(kuchler.tree, Tw)

#wet model ---- 
rasters<-c(kuchler.tree, wet,veg,wood,tree,Tw,Twh,Tg,Tc,Tclx,m,s,d,p3AET,slope,
           sand,SoilpH,hydric,salids,shore,sealevel,elev,bedrock,clay,river)
r.df <- as.data.frame(rasters, xy=TRUE, na.rm=F)
r.df$tree <- ifelse(r.df$Tg <= 3 | r.df$elev > 4000, 0, r.df$tree)
r.df$wood <- ifelse(r.df$Tg <= 3, 0, r.df$wood); r.df$veg <- ifelse(r.df$Tw <= 3, 0, r.df$veg)
r.df <-  subset(r.df,!is.na(sealevel) & !is.na(salids) & !is.na(Tg) & !is.na(Tc) & !is.na(river) &!is.na(elev) & !is.na(slope)  & !is.na(SoilpH) & !is.na(m) & !is.na(sand) & !is.na(clay))
r.df$wt <- ifelse(is.na(r.df$kuchler.tree), 1,100)



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
r.df$tree <- ifelse(r.df$Tg < 3 | r.df$elev > 4000, 0, r.df$tree)
r.df$wood <- ifelse(r.df$Tg < 3, 0, r.df$wood); r.df$veg <- ifelse(r.df$Tw < 3, 0, r.df$veg)
r.df <-  subset(r.df,!is.na(wetmodel)&!is.na(sealevel) & !is.na(salids) & !is.na(Tg) & !is.na(Tc) & !is.na(river) &!is.na(elev) & !is.na(slope)  & !is.na(SoilpH) & !is.na(m) & !is.na(sand) & !is.na(clay))
r.df$wt <- ifelse(is.na(r.df$kuchler.tree), 1,100)



r.df.s <- subset(r.df, !is.na(veg))
#Tw+Twh+Tg+Tc+Tclx+m+s+d+p3AET+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river
rf <- ranger(veg ~ 
               Tw+Twh+Tg+Tc+Tclx+m+s+d+p3AET+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river+wetmodel
             ,
             data=r.df.s, num.trees=25, max.depth = 18, importance = 'impurity', write.forest = TRUE, case.weights = r.df.s$wt)

r.df$output <- predictions(predict(rf, data=r.df))


vegmodel <- rast(cbind(x=r.df$x,y=r.df$y,z=r.df$output), type="xyz", crs=crs(Tw))
plot(vegmodel)
names(vegmodel) <- 'vegmodel'
writeRaster(vegmodel, 'output/vegmodel.tif', overwrite=T)
#wood model ---- 
rasters<-c(kuchler.tree, wet,veg,wood,tree,Tw,Twh,Tg,Tc,Tclx,m,s,d,p3AET,slope,
           sand,SoilpH,hydric,salids,shore,sealevel,elev,bedrock,clay,river,wetmodel)
r.df <- as.data.frame(rasters, xy=TRUE, na.rm=F)
r.df$tree <- ifelse(r.df$Tg < 3 | r.df$elev > 4000, 0, r.df$tree)
r.df$wood <- ifelse(r.df$Tg < 3, 0, r.df$wood); r.df$veg <- ifelse(r.df$Tw < 3, 0, r.df$veg)
r.df <-  subset(r.df,!is.na(wetmodel)&!is.na(sealevel) & !is.na(salids) & !is.na(Tg) & !is.na(Tc) & !is.na(river) &!is.na(elev) & !is.na(slope)  & !is.na(SoilpH) & !is.na(m) & !is.na(sand) & !is.na(clay))
r.df$wt <- ifelse(is.na(r.df$kuchler.tree), 1,100)



r.df.s <- subset(r.df, !is.na(wood))
#Tw+Twh+Tg+Tc+Tclx+m+s+d+p3AET+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river
rf <- ranger(wood ~ 
               Tw+Twh+Tg+Tc+Tclx+m+s+d+p3AET+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river+wetmodel
             ,
             data=r.df.s, num.trees=25, max.depth = 18, importance = 'impurity', write.forest = TRUE, case.weights = r.df.s$wt)

r.df$output <- predictions(predict(rf, data=r.df))


woodmodel <- rast(cbind(x=r.df$x,y=r.df$y,z=r.df$output), type="xyz", crs=crs(Tw))
plot(woodmodel)
names(woodmodel) <- 'woodmodel'
writeRaster(woodmodel, 'output/woodmodel.tif', overwrite=T)

#tree model ---- 
wetmodel<-rast('output/wetmodel.tif')
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
# rasters<-c(kuchler.tree, e,a,p,pAET,Tcl,wet,veg,wood,tree,Tw,Twh,Tg,Tc,Tclx,m,s,d,p3AET,slope,
#            sand,SoilpH,hydric,salids,shore,sealevel,elev,bedrock,clay,river,wetmodel)
r.df <- as.data.frame(rasters, xy=TRUE, na.rm=F)
r.df$tree <- ifelse(r.df$Tg < 3 | r.df$elev > 4000, 0, r.df$tree)
r.df$wood <- ifelse(r.df$Tg < 3, 0, r.df$wood); r.df$veg <- ifelse(r.df$Tw < 3, 0, r.df$veg)
r.df <-  subset(r.df,!is.na(wetmodel)&!is.na(sealevel) & !is.na(salids) & !is.na(Tg) & !is.na(Tc) & !is.na(river) &!is.na(elev) & !is.na(slope)  & !is.na(SoilpH) & !is.na(m) & !is.na(sand) & !is.na(clay))
r.df$wt <- ifelse(is.na(r.df$kuchler.tree), 1,100)



r.df.s <- subset(r.df, !is.na(tree))#& !is.na(kuchler.tree))
#Tw+Twh+Tg+Tc+Tclx+m+s+d+p3AET+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river
rf <- ranger(tree ~ Tw+Twh+Tg+Tc+Tclx+m+s+d+p3AET+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river+wetmodel
             #+shadw0+shadw45+shadw90+shadw135+shadw180+shadw225+shadw270+shadw315
             #m+s+d+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river
             ,
             data=r.df.s, num.trees=25, max.depth = 18, importance = 'impurity', write.forest = TRUE, case.weights = r.df.s$wt)

r.df$output <- predictions(predict(rf, data=r.df))

# r.df.sample <- sample(nrow(r.df.s), nrow(r.df.s)*0.02)
# r.df.ss <- r.df.s[r.df.sample,]
# rf <- randomForest(tree ~ e+a+p+pAET+Tcl+Tw+Twh+Tg+Tc+Tclx+m+s+d+p3AET+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river+wetmodel
#              #+shadw0+shadw45+shadw90+shadw135+shadw180+shadw225+shadw270+shadw315
#              #m+s+d+slope+sand+SoilpH+hydric+salids+shore+sealevel+elev+bedrock+clay+river
#              ,
#              data=r.df.ss, num.trees=25, max.depth = 15, case.weights = r.df.s$wt)
# varImpPlot(rf)

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
r.df$tree <- ifelse(r.df$Tg < 3 | r.df$elev > 4000, 0, r.df$tree)
r.df$wood <- ifelse(r.df$Tg < 3, 0, r.df$wood); r.df$veg <- ifelse(r.df$Tw < 3, 0, r.df$veg)
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
path = 'nam5k/'
Tw <- rast(paste0(path, 'Tw.tif')); names(Tw)<-'Tw'
woodmodel <- rast('output/woodmodel.tif');woodmodel <- crop(woodmodel,Tw);woodmodel <- extend(woodmodel,Tw)
vegmodel <- rast('output/vegmodel.tif');vegmodel <- crop(vegmodel,Tw);vegmodel <- extend(vegmodel,Tw)
treemodel <- rast('output/treemodel.tif');treemodel <- crop(treemodel,Tw);treemodel <- extend(treemodel,Tw)
evermodel <- rast('output/evermodel.tif');evermodel <- crop(evermodel,Tw);evermodel <- extend(evermodel,Tw)

woodmodel <- min(woodmodel, vegmodel)
treemodel <- min(treemodel, woodmodel)
writeRaster(woodmodel, 'output/woodmodel.tif', overwrite=T)
writeRaster(treemodel, 'output/treemodel.tif', overwrite=T)
shrubby <- woodmodel-treemodel
grassy <- vegmodel - woodmodel
openveg <- shrubby+grassy
everveg <- evermodel*woodmodel
deciveg <- (evermodel*-1+1)*woodmodel
writeRaster(shrubby, 'output/shrubby.tif', overwrite=T)
writeRaster(grassy, 'output/grassy.tif', overwrite=T)
writeRaster(openveg, 'output/openveg.tif', overwrite=T)
writeRaster(everveg, 'output/everveg.tif', overwrite=T)
writeRaster(deciveg, 'output/deciveg.tif', overwrite=T)
Sys.time() - systime

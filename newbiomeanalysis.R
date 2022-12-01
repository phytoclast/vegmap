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

# slope1km <- Elev1km %>% terrain(v="slope", neighbors=8, unit="degrees", filename = 'output/slope1km.tif')
# slope5km <- slope1km %>% aggregate(fact=2, fun="mean", na.rm=TRUE, filename="output/slope5km.tif", overwrite=TRUE)
# slope5km <- slope5km %>% project(Tw,filename="output/slope5km.tif", overwrite=TRUE)


slope <- rast("output/slope5km.tif"); names(slope)<-'slope'
rastbrick <- c(Tw,Twh,Tg,Tc,Tclx,m,s,d,e,p3AET,slope, hydric, sealevel,clay, sand,marine,soilpH,bedrock)
eco <- st_read('data/ecoregions.shp')
brown <- st_read('data/biotic_comm_la.shp')
kuchler <- st_read('data/kuchler_DD83.shp')
BPS <- rast('data/BPS.tif') #%>% project(Tw, method='near')
tnbarrens <- rast('data/tnbarrens_modified.tif')



plot(tnbarrens)

brownalt <- read.csv('data/brown.types.cover2.csv')
kuchleralt <- read.csv('data/kuchler.types.cover2.csv')
ecoalt <- read.csv('data/wwfeco.types.cover2.csv')
bpsalt <- read.csv('data/bps.types.cover2.csv')
bpsrat <- foreign::read.dbf('data/BPS.dbf')
bpsrat <- bpsrat %>% left_join(bpsalt)
colnames(bpsrat)


# BPS ----

BPS.pts <- BPS %>% as.data.frame(xy=T) %>% subset(!BPS %in% c(-9999, 11, 12, 31))
BPS.pts.s <- BPS.pts %>% group_by(BPS) %>% sample_n(size=3, replace = TRUE)
BPS.pts.s2 <- BPS.pts[sample(rownames(BPS.pts),size=5000),]
BPS.pts.s <- rbind(BPS.pts.s,BPS.pts.s2)
BPS.pts.sf <- BPS.pts.s %>% st_as_sf(coords = c('x', 'y'), crs=crs(BPS))
BPS.pts <- BPS.pts.sf %>% left_join(bpsrat[,
                      c("VALUE","wet","vegcover","woodycover","treecover" ,"NE","BE","DD","CD" )], by=c("BPS"="VALUE"))
BPS.pts.repro <- BPS.pts %>% sf::st_transform(crs = crs(Tw))


tnbarrens.1 <-   ifel(tnbarrens$tnbarrens_modified_2 ==0, 1, 0)
newext <- ext(tnbarrens)+c(-20000,-10000,-10000,-100000)
tnbarrens.1 <- crop(tnbarrens.1, newext) %>% project(Tw)
plot(tnbarrens.1)

# kuchler ----

kuchlerids <- kuchler$TYPE %>% unique()
for(i in 1:length(kucids)){#i=326
  kuchler0 <- kuchler %>% subset(TYPE %in% kuchlerids[i]) %>% vect()
  kuchlerpts0 <- spatSample(kuchler0, size=5, "random", strata=NULL)
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
  ecopts0 <- spatSample(eco0, size=5, "random", strata=NULL)
  while(nrow(ecopts0) == 0){ecopts0 <- spatSample(eco0, size=5, "random", strata=NULL)}
  if(i==1){ecopts <- ecopts0}else{
    ecopts <- rbind(ecopts, ecopts0)} 
}
ecopts0 <- terra::spatSample(vect(eco), size=10000, "random", strata=NULL)
ecopts <- rbind(ecopts,ecopts0)
ecopts.repro <- ecopts %>% project(Tw)
ecopts.repro <- ecopts.repro %>% st_as_sf() %>%  left_join(ecoalt)


# brown ----

# (brown$BIOMENAME1) %>% unique()
brown <- brown %>% subset(!BIOMENAME1 %in% c("Permanent Ice and Snow","Open Water Lakes"))
brownids <- brown$BIOMENAME1 %>% unique()
for(i in 1:length(brownids)){#i=326
  brown0 <- brown %>% subset(BIOMENAME1 %in% brownids[i]) %>% vect()
  brownpts0 <- spatSample(brown0, size=5, "random", strata=NULL)
  while(nrow(brownpts0) == 0){brownpts0 <- spatSample(brown0, size=10, "random", strata=NULL)}
  if(i==1){brownpts <- brownpts0}else{
    brownpts <- rbind(brownpts, brownpts0)} 
}
brownpts0 <- terra::spatSample(vect(brown), size=5000, "random", strata=NULL)
brownpts <- rbind(brownpts,brownpts0)
brownpts.repro <- brownpts %>% project(Tw)
brownpts.repro <- brownpts.repro %>% st_as_sf() %>%  left_join(brownalt)

#process layer sampling ----
commoncols <- c("wet","vegcover","woodycover","treecover" ,"NE","BE","DD","CD" )

ecopts.all <- ecopts.repro[,commoncols] %>% dplyr::bind_rows(BPS.pts.repro[,commoncols])%>% 
dplyr::bind_rows(kuchlerpts.repro[,commoncols])%>% 
dplyr::bind_rows(brownpts.repro[,commoncols])
 
 plot(vect(ecopts.all))
 





ecopts1 <- rastbrick %>% extract(ecopts.all)
ecopts2 <- ecopts.all
ecopts3 <- cbind(ecopts2, ecopts1)
# ecopts3 <- ecopts3[,-26]
# ecopts3 <- ecopts3 %>% left_join(ecoalt[,c('ECO_ID','altbiome','altbiome2')])
ecopts3 <- subset(ecopts3,!is.na(Tg) &  !is.na(Tc) &  !is.na(slope) &  !is.na(hydric) & !is.na(sand) & !is.na(m))
ecopts3 <- plot(vect(ecopts3))
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
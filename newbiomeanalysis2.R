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
rastbrick <- c(Tw,Twh,Tg,Tc,Tclx,m,s,d,e,p3AET,slope, hydric, elev, sealevel,clay, sand,marine,soilpH,bedrock)


eco <- st_read('data/ecoregions.shp')
ecoalt <- read.csv('data/wwfeco.types.cover2.csv')
biome2023 <- read.csv('data/biome2023.csv',  na.strings = FALSE, fileEncoding = 'latin1')
eco <- eco %>% subset(!BIOME %in% c(98))
ecoids <- eco$ECO_ID %>% unique()


worldbiome <- st_read('data/biomes_world.shp') |> subset(!is.na(biome))
subbiomes <- worldbiome$subbiome |> unique() 


for(i in 1:length(subbiomes)){#i=326
  eco0 <- worldbiome %>% subset(subbiome %in% subbiomes[i]) %>% vect()
  ecopts0 <- spatSample(eco0, size=2000, "random", strata=NULL)
  while(nrow(ecopts0) == 0){ecopts0 <- spatSample(eco0, size=10, "random", strata=NULL)}
  if(i==1){ecopts <- ecopts0}else{
    ecopts <- rbind(ecopts, ecopts0)} 
}
ecopts0 <- terra::spatSample(vect(worldbiome), size=40000, "random", strata=NULL)
ecopts <- rbind(ecopts,ecopts0)
ecopts.repro <- ecopts %>% project(Tw)
ecopts.repro <- ecopts.repro %>% st_as_sf() 

ecopts1 <- rastbrick %>% extract(ecopts.repro)
ecopts2 <- ecopts.repro %>% st_drop_geometry()
ecopts3 <- cbind(ecopts2, ecopts1)

ecopts3 <-  subset(ecopts3,  !is.na(Tw) & !is.na(m) & !is.na(Tclx) & !is.na(Tg) &!is.na(p3AET) &!is.na(slope) &!is.na(hydric) &!is.na(sand) 
                   & !(Tg > 9 & subbiome %in% c('1a', '1b', '1c'))
                   & !(Tg < 3 & !subbiome %in% c('1a', '1b', '1c'))
                   )
# ecopts3 <- ecopts3 |> mutate(var1 = as.factor(subbiome), var1 = as.numeric(var1))
ecopts3 <- ecopts3 |> mutate(altbiom2023 = case_when(subbiome %in% '1a' ~ 1.1,
                                              subbiome %in% '1b' ~ 1.2,
                                              subbiome %in% '1c' ~ 1.3,
                                              subbiome %in% '2a' ~ 2.1,
                                              subbiome %in% '2b' ~ 2.2,
                                              subbiome %in% '3a' ~ 3.1,
                                              subbiome %in% '4a' ~ 4.1,
                                              subbiome %in% '4b' ~ 4.2,
                                              subbiome %in% '4c' ~ 4.3,
                                              subbiome %in% '5a' ~ 5.1,
                                              subbiome %in% '5b' ~ 5.2,
                                              subbiome %in% '5c' ~ 5.3,
                                              subbiome %in% '6a' ~ 6.1,
                                              subbiome %in% '6b' ~ 6.2,
                                              subbiome %in% '7a' ~ 7.1,
                                              subbiome %in% '7b' ~ 7.2,
                                              subbiome %in% '7c' ~ 7.3,
                                              subbiome %in% '8a' ~ 8.1,
                                              subbiome %in% '8b' ~ 8.2,
                                              subbiome %in% '9a' ~ 9.1
))
# ecopts3 <- ecopts3 |> mutate(var1 = case_when(subbiome %in% '1a' ~ 2,
#                                               subbiome %in% '1b' ~ 2,
#                                               subbiome %in% '1c' ~ 2,
#                                               subbiome %in% '2a' ~ 2,
#                                               subbiome %in% '2b' ~ 2,
#                                               subbiome %in% '3a' ~ 2,
#                                               subbiome %in% '4a' ~ 2,
#                                               subbiome %in% '4b' ~ 2,
#                                               subbiome %in% '4c' ~ 2,
#                                               subbiome %in% '5a' ~ 2,
#                                               subbiome %in% '5b' ~ 2,
#                                               subbiome %in% '5c' ~ 2,
#                                               subbiome %in% '6a' ~ 2,
#                                               subbiome %in% '6b' ~ 2,
#                                               subbiome %in% '7a' ~ 1,
#                                               subbiome %in% '7b' ~ 1,
#                                               subbiome %in% '7c' ~ 1,
#                                               subbiome %in% '8a' ~ 2,
#                                               subbiome %in% '8b' ~ 2,
#                                               subbiome %in% '9a' ~ 2
# ))

# unique(subset(ecopts3, select=c(altbiom2023, subbiome)))
# 
# rf <- ranger(altbiom2023 ~ 
#                Tc+Tclx+m+s+d+p3AET+Tg+Tw+Twh+e+
#                slope+hydric+sealevel+clay+sand+marine+soilpH+bedrock
#              ,
#              data=ecopts3, num.trees=100, sample.fraction = 0.01, max.depth = 50, importance = 'impurity',
#              classification=TRUE,  write.forest = TRUE)
# 
# vimp <- data.frame(imp = rf$variable.importance) |> mutate(var = names(rf$variable.importance))  |> arrange(by=imp) 
# library(ggplot2)
# ggplot(data=vimp, aes(y=imp, x=factor(var, levels = vimp$var)))+
#   geom_col()+coord_flip()+scale_x_discrete(name='variable')+scale_y_discrete(name='importance')
# 
# 
# 
# vegmodel <- predict(object=rastbrick,  model=rf, na.rm=TRUE)
# vegmodel1 <- vegmodel$prediction
# plot(vegmodel1)
# names(vegmodel1) <- 'biome2023'
# vegmodel1 <- extend(vegmodel1, Tw); vegmodel1 <- crop(vegmodel1, Tw)
# # plot(rastbrick$d)
# writeRaster(vegmodel1, 'output/global/biome2023b.tif', overwrite=T)



#----




for(i in 1:length(ecoids)){#i=326
  eco0 <- eco %>% subset(ECO_ID %in% ecoids[i]) %>% vect()
  ecopts0 <- spatSample(eco0, size=10, "random", strata=NULL)
  while(nrow(ecopts0) == 0){ecopts0 <- spatSample(eco0, size=10, "random", strata=NULL)}
  if(i==1){ecopts <- ecopts0}else{
    ecopts <- rbind(ecopts, ecopts0)} 
}
ecopts0 <- terra::spatSample(vect(eco), size=60000, "random", strata=NULL)
ecopts1 <- terra::spatSample(vect(subset(eco, REALM %in% 'NA' )), size=10000, "random", strata=NULL)
ecopts <- rbind(ecopts,ecopts0, ecopts1)
ecopts.repro <- ecopts %>% project(Tw)
ecopts.repro <- ecopts.repro %>% st_as_sf() %>%  left_join(biome2023) |>  subset(select='altbiom2023')
  #mutate(m=NULL,p3AET=NULL,Tg=NULL,Tcc=NULL)

ecopts1 <- rastbrick %>% extract(ecopts.repro)
ecopts2 <- ecopts.repro %>% st_drop_geometry()
ecopts4 <- cbind(ecopts2, ecopts1)
ecopts3 <- subset(ecopts3, select=c(altbiom2023,Tw,Twh,Tg,Tc,Tclx,m,s,d,e,p3AET,
                                    slope,hydric,sealevel,clay,sand,marine,soilpH,bedrock))
ecopts4 <- subset(ecopts4, select=c(altbiom2023,Tw,Twh,Tg,Tc,Tclx,m,s,d,e,p3AET,
                                    slope,hydric,sealevel,clay,sand,marine,soilpH,bedrock))


ecopts5 <- rbind(ecopts4, ecopts3)
ecopts5 <-  subset(ecopts5, !is.na(altbiom2023) & !is.na(Tw) & !is.na(m) & !is.na(Tclx) & !is.na(Tg) &!is.na(p3AET) &!is.na(slope) &!is.na(hydric) &!is.na(sand) & !(Tg > 9 & altbiom2023 %in% c(1.1,1.2,1.3))
                   & !(Tg < 3 & !altbiom2023 %in% c(1.1,1.2,1.3))
                   & !(m > 1 & altbiom2023 %in% c(7.1,7.2,7.3))
                   & !(m < 0.5 & altbiom2023 %in% c(2.1,2.2,3.1,4.1,4.2,4.3,9.1))
                   & !(Tclx < -2 & altbiom2023 %in% c(9.1))
                   & !(Tclx > 5 & altbiom2023 %in% c(1.1,1.2,2.1,2.2,3.1)))
rf <- ranger(altbiom2023 ~ 
               Tw+Twh+Tg+Tc+Tclx+m+s+d+e+p3AET+
               slope+hydric+sealevel+clay+sand+marine+soilpH+bedrock
             ,
             data=ecopts5, num.trees=50, sample.fraction = 0.1, max.depth = 18, importance = 'impurity',
             classification=TRUE,  write.forest = TRUE)
vimp <- data.frame(imp = rf$variable.importance) |> mutate(var = names(rf$variable.importance))  |> arrange(by=imp) 
library(ggplot2)
ggplot(data=vimp, aes(y=imp, x=factor(var, levels = vimp$var)))+
  geom_col()+coord_flip()+scale_x_discrete(name='variable')+scale_y_discrete(name='importance')

vegmodel <- predict(object=rastbrick,  model=rf, na.rm=TRUE)
vegmodel1 <- vegmodel$prediction
plot(vegmodel1)
names(vegmodel1) <- 'biome2023'
vegmodel1 <- extend(vegmodel1, Tw); vegmodel1 <- crop(vegmodel1, Tw)
# plot(rastbrick$d)
writeRaster(vegmodel1, 'output/global/biome2023.tif', overwrite=T)

# ----
library(rpart)
library(rpart.plot)
unique(ecopts5$altbiom2023) |> sort()
ecopts6 <- ecopts5 |> mutate(key1 = case_when(altbiom2023 %in% 1.1 ~ 1,
                                              altbiom2023 %in% 1.2 ~ 1,
                                              altbiom2023 %in% 1.3 ~ 1,
                                              altbiom2023 %in% 2.1 ~ 2,
                                              altbiom2023 %in% 2.2 ~ 2,
                                              altbiom2023 %in% 3.1 ~ 3,
                                              altbiom2023 %in% 4.1 ~ 4,
                                              altbiom2023 %in% 4.2 ~ 4,
                                              altbiom2023 %in% 4.3 ~ 4,
                                              altbiom2023 %in% 5.1 ~ 5,
                                              altbiom2023 %in% 5.2 ~ 5,
                                              altbiom2023 %in% 5.3 ~ 5,
                                              altbiom2023 %in% 6.1 ~ 6,
                                              altbiom2023 %in% 6.2 ~ 6,
                                              altbiom2023 %in% 7.1 ~ 7,
                                              altbiom2023 %in% 7.2 ~ 7,
                                              altbiom2023 %in% 7.3 ~ 7,
                                              altbiom2023 %in% 8.1 ~ 8,
                                              altbiom2023 %in% 8.2 ~ 8,
                                              altbiom2023 %in% 9.1 ~ 9
))
ecopts6 <- subset(ecopts6, altbiom2023 %in% c(1.1, 1.2, 1.3, 2.1, 2.2, 3.1, 4.1, 4.2, 4.3, 5.1, 5.2, 5.3, 6.1, 6.2, 7.1, 7.2, 7.3, 8.1, 8.2, 9.1))
# ecopts6 <- subset(ecopts6, altbiom2023 %in% c(5.1,8.1))



biomeclass <- rpart(key1 ~ 
                      m+p3AET+d+
                      Tg+Tc+Tclx 
                    #+Tw+Twh
                    # +s+slope
                    # +hydric+sealevel+clay+sand+marine+soilpH+bedrock
                    ,
                    data = ecopts6, method="class", control = list(maxdepth = 6, cp=0.002, minsplit=100))
# Make plot

png(filename="biome2023.png",width = 10, height = 3, units = 'in', res = 600)

rpart.plot(biomeclass, extra=108,legend.cex=0.5, digits=3) # Make plot

dev.off()


#----

ecopts6 <- subset(ecopts5, altbiom2023 %in% c(1.1, 1.2, 1.3, 2.1, 2.2, 3.1, 4.1, 4.2, 4.3, 9.1) & m >= 1)
ecopts6 <- subset(ecopts5, altbiom2023 %in% c(6.2,5.2))
ecopts6 <- ecopts6 |> mutate(t15 = pmin(Tc,Tclx+15),t12 = pmin(Tc,Tclx+12),t18 = pmin(Tc,Tclx+18))
rf <- ranger(altbiom2023 ~ 
               m+p3AET+d
               #+Tg+Tc+Tclx+Tw+Twh +slope
             ,
             data=ecopts6, num.trees=200, sample.fraction = 0.01, max.depth = 10, importance = 'impurity',
             classification=TRUE,  write.forest = TRUE)
vimp <- data.frame(imp = rf$variable.importance) |> mutate(var = names(rf$variable.importance))  |> arrange(by=imp) 
library(ggplot2)
ggplot(data=vimp, aes(y=imp, x=factor(var, levels = vimp$var)))+
  geom_col()+coord_flip()+scale_x_discrete(name='variable')+scale_y_discrete(name='importance')
rf$prediction.error

lm(Tclx ~ Tc, data=ecopts6)
lm(Tg ~ Tc, data=ecopts6)
# ggplot(ecopts6, aes(x=Tclx,y=Tc))+
#   geom_point(alpha=0.1)+
#   geom_smooth()

Tc.matrix <- matrix(data=seq(-45, 25, (25--45)/280), nrow=321, ncol=281, byrow = TRUE) |> rast()
Tg.matrix <- matrix(data=seq(40, 0, -(40-0)/320), nrow=321, ncol=281, byrow = FALSE) |> rast()
randomatrix <- matrix(data=runif(321*281, -30,15), nrow=321, ncol=281, byrow = TRUE) |> rast()
Tclx.matrix <- Tc.matrix*1.306 + -17.009 + randomatrix*0.5

Tc.matrix <- ifel(Tc.matrix <= max(ecopts6$Tc),Tc.matrix,NA)
Tc.matrix <- ifel(Tc.matrix >= min(ecopts6$Tc),Tc.matrix,NA)
Tg.matrix <- ifel(Tg.matrix <= max(ecopts6$Tg),Tg.matrix,NA)
Tg.matrix <- ifel(Tg.matrix >= min(ecopts6$Tg),Tg.matrix,NA)
Tclx.matrix <- ifel(Tclx.matrix <= max(ecopts6$Tclx),Tclx.matrix,NA)
Tclx.matrix <- ifel(Tclx.matrix >= min(ecopts6$Tclx),Tclx.matrix,NA)
Tc.matrix <- ifel(Tc.matrix > Tg.matrix,NA, Tc.matrix)
Tclx.matrix <- ifel(Tclx.matrix > Tc.matrix,NA, Tclx.matrix)
t18 <- min(Tc.matrix, Tclx.matrix+18)
t15 <- min(Tc.matrix, Tclx.matrix+15)
t12 <- min(Tc.matrix, Tclx.matrix+12)
brickmatrix <- c(Tg.matrix, Tc.matrix, Tclx.matrix, t15, t12, t18)
names(brickmatrix) <- c('Tg','Tc','Tclx','t15', 't12', 't18')
plot(Tclx.matrix)
vegmodel <- predict(object=brickmatrix,  model=rf, na.rm=TRUE)
vegmodel1 <- vegmodel$prediction
plot(vegmodel1)
newmatrix <- c(vegmodel1, brickmatrix) |> as.matrix() |> as.data.frame()

biomeclass <- rpart(altbiom2023 ~ 
                      m+p3AET+d+
                      Tg+Tc+Tclx
                    ,
                    data = ecopts6, method="class", control = list(maxdepth = 6, cp=0.005, minsplit=100))
# Make plot

png(filename="biome2023a.png",width = 10, height = 3, units = 'in', res = 600)

rpart.plot(biomeclass, extra=108,legend.cex=0.5, digits=3) # Make plot

dev.off()


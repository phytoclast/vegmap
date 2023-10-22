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


month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
for(i in 1:12){
  t0 = rast(paste0('C:/workspace2/processclimategrids/output/amplified/t',month[i],'.tif'))
  th0 = rast(paste0('C:/workspace2/processclimategrids/output/amplified/th',month[i],'.tif'))
  tl0 = rast(paste0('C:/workspace2/processclimategrids/output/amplified/tl',month[i],'.tif'))
  p0 = rast(paste0('C:/workspace2/processclimategrids/data/p',month[i],'.tif'))
  e0 = rast(paste0('C:/workspace2/processclimategrids/output/e',month[i],'.tif'))
  names(t0) = paste0('t',month[i])
  names(th0) = paste0('th',month[i])
  names(tl0) = paste0('tl',month[i])
  names(p0) = paste0('p',month[i])
  names(e0) = paste0('e',month[i])
  assign(paste0('t',month[i]), t0)
  assign(paste0('th',month[i]), th0)
  assign(paste0('tl',month[i]), tl0)
  assign(paste0('p',month[i]), p0)
  assign(paste0('e',month[i]), e0)
if(i == 1){
  tt = get(paste0('t',month[i]))
  tth = get(paste0('th',month[i]))
  ttl = get(paste0('tl',month[i]))
  pp = get(paste0('p',month[i]))
  pp = get(paste0('p',month[i]))
  ee = get(paste0('e',month[i]))
}else{
  tt = c(tt,get(paste0('t',month[i])))
  tth = c(tth,get(paste0('th',month[i])))
  ttl = c(ttl,get(paste0('tl',month[i])))
  pp = c(pp,get(paste0('p',month[i])))
  ee = c(ee,get(paste0('e',month[i])))
}
}

for(i in 1:12){
  p0 = get(paste0('p',month[i]))
  e0 = get(paste0('e',month[i]))
  a0 = min(e0,p0)
  assign(paste0('a',month[i]), a0)
}

# a.sum = sum(a01,a02,a03,a04,a05,a06,a07,a08,a09,a10,a11,a12)
# e.sum = sum(e01,e02,e03,e04,e05,e06,e07,e08,e09,e10,e11,e12)
# p.sum = sum(p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,p11,p12)
# 
# plot(a.sum)
# writeRaster(a.sum,'output/a.sum.tif', overwrite=T)
# ae.ratio <- a.sum/(e.sum+0.0001)
# plot(ae.ratio)
# writeRaster(ae.ratio,'output/ae.ratio.tif', overwrite=T)
# 
# pe.ratio <- p.sum/(e.sum+0.0001)
# pe.ratio <- ifel(pe.ratio >1,1,pe.ratio)
# plot(pe.ratio)
# writeRaster(ae.ratio,'output/pe.ratio.tif', overwrite=T)
# 
# aepe.ratio <- pe.ratio-ae.ratio
# plot(aepe.ratio)


#----
eco <- st_read('data/ecoregions.shp')
biome2023 <- read.csv('data/biome2023.csv',  na.strings = FALSE, fileEncoding = 'latin1') |> subset(select=c(ECO_ID, altbiom2023)) |> unique()
eco <- eco %>% subset(!BIOME %in% c(98,99))
ecoids <- eco$ECO_ID %>% unique()
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
ecopts.repro <- ecopts.repro %>% st_as_sf() 
ecopts.xy <- st_coordinates(ecopts.repro)
ecopts.repro <- ecopts.repro %>%  left_join(biome2023) |>  cbind(ecopts.xy) |>  
  subset(select=c(ECO_ID, ECO_CODE, ECO_NAME, BIOME_NAME, NEWRELM, altbiom2023, X,Y))
#mutate(m=NULL,p3AET=NULL,Tg=NULL,Tcc=NULL)
rastbrick <- c(t01,t02,t03,t04,t05,t06,t07,t08,t09,t10,t11,t12,
               th01,th02,th03,th04,th05,th06,th07,th08,th09,th10,th11,th12,
               tl01,tl02,tl03,tl04,tl05,tl06,tl07,tl08,tl09,tl10,tl11,tl12,
               p01,p02,p03,p04,p05,p06,p07,p08,p09,p10,p11,p12,
  Tw,Twh,Tg,Tc,Tclx,m,s,d,e,p3AET,slope, hydric, elev, sealevel,clay, sand,marine,soilpH,bedrock)

ecopts1 <- rastbrick %>% extract(ecopts.repro)
ecopts2 <- ecopts.repro %>% st_drop_geometry()
ecopts4 <- cbind(ecopts2, ecopts1)
ecopts5 <-  subset(ecopts4,  !is.na(Tw) & !is.na(m) & !is.na(Tclx) & !is.na(Tg) &!is.na(p3AET) &!is.na(slope) &!is.na(hydric) &!is.na(sand))

saveRDS(ecopts5,'ecopoints.RDS')
colnames(ecopts5)
ecoptsmed <- ecopts5 |> group_by(ECO_ID,ECO_CODE,ECO_NAME,BIOME_NAME,NEWRELM,altbiom2023) |> summarise(across(c("X","Y","t01", "t02", "t03", "t04", "t05", "t06", "t07", "t08", "t09", "t10", "t11", "t12", "th01", "th02", "th03", "th04", "th05", "th06", "th07", "th08", "th09", "th10", "th11", "th12", "tl01", "tl02", "tl03", "tl04", "tl05", "tl06", "tl07", "tl08", "tl09", "tl10", "tl11", "tl12", "p01", "p02", "p03", "p04", "p05", "p06", "p07", "p08", "p09", "p10", "p11", "p12", "Tw", "Twh", "Tg", "Tc", "Tclx", "m", "s", "d", "e", "p3AET", "slope", "hydric", "elev", "sealevel", "clay", "sand", "marine", "soilpH", "bedrock"), median))

saveRDS(ecoptsmed,'ecoptsmed.RDS')

library(terra)
library(sf)
library(climatools)
elev <- rast("C:/scripts/processclimategrids/output/Elev1km.tif")
elev5 <- rast("C:/scripts/processclimategrids/output/Elev5km.tif")


eco <-  st_read('C:/a/geo/ecoregion/ecoregions.shp') |> subset(ECO_CODE %in% 'LAKE')
eco <-  eco |> mutate(lat = st_coordinates(st_centroid(st_geometry(eco)))[,2],lon = st_coordinates(st_centroid(st_geometry(eco)))[,1], a=st_area(st_geometry(eco)))
eco <-  eco |> subset(lat < 0 | lon > -55 & unclass(a) < 200000000000)
ocean <- st_read('C:/a/geo/base/ne_10m_ocean.shp')
ocean <- ocean |> st_cast("POLYGON")
america <- st_read('C:/a/geo/base/americas3.shp') |> subset(COLOR %in% 'Blue or Delete')
greatlakes <-st_read('C:/a/geo/base/greatlakes.shp')
america <- america |> mutate(water=1) |> st_make_valid() |> select(water)
ocean <- ocean |> mutate(water=1) |> select(water)
greatlakes <- greatlakes |> mutate(water=1)|>  st_make_valid() |> select(water)
eco <- eco |> mutate(water=1)|>  st_make_valid() |> select(water)

wbody <- st_as_sf(rbind(ocean,greatlakes,eco,america)) 
rwbody <- rasterize(vect(wbody), field='water',y=elev)
rwb <- ifel(is.na(rwbody), 0,1)
writeRaster(rwb, 'C:/scripts/vegmap/global/water.tif')
#######
library(terra)
library(sf)
library(climatools)

w <- rast('C:/scripts/vegmap/global/water.tif')
plot(w)
wrp <- climatools::reproject(w,prj = setProjection("equaldistant.cylindrical",lat=0,lon=0,orglat=0))
w50 <- focalmed(wrp, r=50000)
w50 <- project(w50, w)
names(w50) <- 'w50'
plot(w50)
writeRaster(w50, 'C:/scripts/vegmap/global/w50.tif')
w500 <- focalmed(wrp, r=500000)
w500 <- project(w500, w)
names(w500) <- 'w500'
writeRaster(w500, 'C:/scripts/vegmap/global/w500.tif')
plot(w500)
w5000 <- focalmed(wrp, r=5000000)
w5000 <- project(w5000, w)
names(w5000) <- 'w5000'
writeRaster(w5000, 'C:/scripts/vegmap/global/w5000.tif')
plot(w5000)
wx <- (1-w50+1)*(1-w500+1)
elev <- rast("C:/scripts/processclimategrids/output/Elev1km.tif")
names(elev)<-'elev'

###############
library(terra)
library(sf)
library(climatools)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

elev <- rast("C:/scripts/processclimategrids/output/Elev1km.tif")
names(elev)<-'elev'
w50 <- rast('C:/scripts/vegmap/global/w50.tif')
w500 <- rast('C:/scripts/vegmap/global/w500.tif')
w5000 <- rast('C:/scripts/vegmap/global/w5000.tif')
br <- c(elev,w50,w500,w5000)

stations <- read.fwf('ghcn/ghcnd-stations.txt', widths = c(11,9,10,7,31))
colnames(stations) <- c('ID','LATITUDE','LONGITUDE','STNELEV','NAME')          

stations <- st_as_sf(stations,coords=c(x='LONGITUDE',y='LATITUDE'), crs=crs(br), remove=F)
brextract <- extract(br,vect(stations))
stbr <- cbind(stations, brextract)
stbr <- subset(stbr,select=-ID.1)
stbr <- stbr |> st_drop_geometry()
write.csv(stbr, 'ghcn/stbr.csv', row.names = F)



stsel <- stbr |> subset(elev-STNELEV >  1000 & STNELEV != -999.9)


#############
library(terra)
library(sf)
library(climatools)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

br2 <- rast('ghcn/br.tif')

w <- br2$w500
w3 <- aggregate(w, 3)
ww <- reproject(w, prj = setProjection("equaldistant.cylindrical",lat=0,lon=0,orglat=0)) |> focalmed(r=500000) |> project(w)
www <- reproject(w, prj = setProjection("equaldistant.cylindrical",lat=0,lon=0,orglat=0)) |> focalmed(r=500000) |> focalmed(r=500000)|> focalmed(r=500000)|> focalmed(r=500000)|> focalmed(r=500000) |> project(w)
plot(ww)
ww2 <- (ww+www)/2
asp <- terrain(ww2, v="aspect", neighbors=8, unit="radians")  
# slop <- terrain(ww2, v="slope", neighbors=8, unit="radians")  
edg <- 1-2*((0.5-ww2)^2)^0.5
# plot(edg)
coasteasting <- (sin(asp)*edg)*-1
coastnorthing <- (cos(asp)*edg)*-1
plot(coasteasting)

#arctic/antarctic
lon=seq(-180,180,10)
lat=85
direction=180
windtab <- data.frame(lat=lat,lon=lon,direction=direction)
lon=seq(-180,180,10)
lat=-85
direction=000
windtab0 <- data.frame(lat=lat,lon=lon,direction=direction)
windtab <- rbind(windtab,windtab0)
#equator
lon=seq(-180,-25,10)
lat=0
direction=90
windtab0 <- data.frame(lat=lat,lon=lon,direction=direction)
windtab <- rbind(windtab,windtab0)
lon=seq(100,180,10)
lat=0
direction=90
windtab0 <- data.frame(lat=lat,lon=lon,direction=direction)
windtab <- rbind(windtab,windtab0)
lon=seq(50,100,10)
lat=0
direction=180
windtab0 <- data.frame(lat=lat,lon=lon,direction=direction)
windtab <- rbind(windtab,windtab0)
#westerlies
lon=seq(-180,180,10)
lat=45
direction=225
windtab0 <- data.frame(lat=lat,lon=lon,direction=direction)
windtab <- rbind(windtab,windtab0)
lon=seq(-180,180,10)
lat=-45
direction=315
windtab0 <- data.frame(lat=lat,lon=lon,direction=direction)
windtab <- rbind(windtab,windtab0)
#trades
lon=seq(-180,180,10)
lat=15
direction=45
windtab0 <- data.frame(lat=lat,lon=lon,direction=direction)
windtab <- rbind(windtab,windtab0)
lon=seq(-100,180,10)
lat=15
direction=45
windtab0 <- data.frame(lat=lat,lon=lon,direction=direction)
windtab <- rbind(windtab,windtab0)
lon=seq(-180,180,10)
lat=-15
direction=135
windtab0 <- data.frame(lat=lat,lon=lon,direction=direction)
windtab <- rbind(windtab,windtab0)
lon=seq(50,100,10)
lat=15
direction=225
windtab0 <- data.frame(lat=lat,lon=lon,direction=direction)
windtab <- rbind(windtab,windtab0)

windtab <- windtab |> mutate(rad = (direction/360)*2*pi, easting=sin(rad), northing=cos(rad))
library(fields)
xy <- windtab[,c('lon','lat')] |> st_drop_geometry() |> as.data.frame()
v <- windtab$easting
tps <- Tps(xy, v, lon.lat = TRUE)
easting <- interpolate(rast(w3), tps, xyOnly=TRUE)#
v <- windtab$northing
tps <- Tps(xy, v, lon.lat = TRUE)
northing <- interpolate(rast(w3), tps, xyOnly=TRUE)#
plot(northing)
easting <- easting |> project(w)
northing <- northing |> project(w)
# easting <- ifel(easting >1, 1, ifel(easting<0,0,easting))
# northing <- ifel(northing >1, 1, ifel(northing<0,0,northing))
east <- (easting*1+coasteasting)/2#
nort <- (northing*1+coastnorthing)/2#
# east <- coasteasting
# nort <- coastnorthing

swx = (1/2)^0.5*-1
swy = (1/2)^0.5*-1
windtab <- windtab |> mutate(sw = 1-(((swx-easting)/2)^2+((swy-northing)/2)^2)^0.5)

w.n <- 1-(((0-east)/2)^2+((1-nort)/2)^2)^0.5
w.e <- 1-(((1-east)/2)^2+((0-nort)/2)^2)^0.5
w.s <- 1-(((0-east)/2)^2+((-1-nort)/2)^2)^0.5
w.w <- 1-(((-1-east)/2)^2+((0-nort)/2)^2)^0.5
w.ne <- 1-(((1*(1/2)^0.5-east)/2)^2+((1*(1/2)^0.5-nort)/2)^2)^0.5
w.se <- 1-(((1*(1/2)^0.5-east)/2)^2+((-1*(1/2)^0.5-nort)/2)^2)^0.5
w.sw <- 1-(((-1*(1/2)^0.5-east)/2)^2+((-1*(1/2)^0.5-nort)/2)^2)^0.5
w.nw <- 1-(((-1*(1/2)^0.5-east)/2)^2+((1*(1/2)^0.5-nort)/2)^2)^0.5

w.n <- (ifel(w.n>=0.5,w.n,0.5)-0.5)*2
w.e <- (ifel(w.e>=0.5,w.e,0.5)-0.5)*2
w.s <- (ifel(w.s>=0.5,w.s,0.5)-0.5)*2
w.w <- (ifel(w.w>=0.5,w.w,0.5)-0.5)*2
w.ne <- (ifel(w.ne>=0.5,w.ne,0.5)-0.5)*2
w.se <- (ifel(w.se>=0.5,w.se,0.5)-0.5)*2
w.sw <- (ifel(w.sw>=0.5,w.sw,0.5)-0.5)*2
w.nw <- (ifel(w.nw>=0.5,w.nw,0.5)-0.5)*2
plot(w.ne*(wind$wind045-1))

wind <-rast('ghcn/wind.tif')
windmodel <- w.n*(wind$wind000-1)+w.e*(wind$wind090-1)+w.s*(wind$wind180-1)+w.w*(wind$wind270-1)+
  w.ne*(wind$wind045-1)+w.se*(wind$wind135-1)+w.sw*(wind$wind225-1)+w.nw*(wind$wind315-1)
writeRaster(windmodel, 'ghcn/windmodel.tif', overwrite=T)
plot(windmodel)

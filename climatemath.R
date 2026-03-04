library(terra)
library(sf)
library(climatools)
library(dplyr)
library(fields)
library(gstat)
library(gam)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
logp <- function(x){log10(x+1)}
logtr <- function(x){log10(x+0.01)}
alogp <- function(x){
  x <- 10^x-1
  x <- ifel(x < 0,0,x)
  return(x)}
alogtr <- function(x){
  x <- 10^x-0.01
  x <- ifel(x < 0,0,x)
  return(x)}

parm = 't'
mo = 7
mos <- c("01","02","03","04","05","06","07","08","09","10","11","12")
for(mo in 1:12){
  parm = 't'
  nameofpar <- paste0(parm,mos[mo])
  filnam <- paste0('clim1990/',nameofpar,'.tif')
  x <- rast(filnam)
  assign(nameofpar, x)
}
for(mo in 1:12){
  parm = 'tr'
  nameofpar <- paste0(parm,mos[mo])
  filnam <- paste0('clim1990/',nameofpar,'.tif')
  x <- rast(filnam)
  assign(nameofpar, x)
}
for(mo in 1:12){
  parm = 'p'
  nameofpar <- paste0(parm,mos[mo])
  filnam <- paste0('clim1990/',nameofpar,'.tif')
  x <- rast(filnam)
  assign(nameofpar, x)
}
br.t <- rast(mget(paste0('t',mos)))
br.tr <- rast(mget(paste0('tr',mos)))
br.p <- rast(mget(paste0('p',mos)))
br.tr <- alogtr(br.tr)
br.p <- alogp(br.p)
br.th <- br.t + br.tr/2
br.tl <- br.t - br.tr/2
names(br.th) <- paste0('th',mos)
names(br.tl) <- paste0('tl',mos)
GrowTemp.rast0 <- function(block, t.jan='t01'){
  t.ind = which(names(block) %in% t.jan)
  t.val <- block[,,t.ind:(t.ind+11),drop=FALSE]
  t.val <- ifel(t.val>0,t.val,0)
  t.val <- max(mean(t.val[,,c(11,12,1,2,3,4),drop=FALSE]),mean(t.val[,,c(5,6,7,8,9,10),drop=FALSE]))
  names(t.val) <- 'Tg'
  return(t.val)
}
Tg <- GrowTemp.rast0(br.t)
block <- br.t
writeRaster(Tg, 'clim1990/Tg.tif')

elev <- rast('global/br5000.tif')
elev <- elev$elev |> project(br.t)
x <- elev
xy <- rast(nrows=180, ncols=360, nlyrs=1, crs=crs(x), extent=ext(x), vals=1) |> project(crs('EPSG:4326'))
xy <- terra::as.data.frame(x=xy, xy=TRUE)
lon <- rast(x=xy[,c('x','y','x')], type="xyz", crs=crs(x), extent=ext(x)) |> project(x) 
lat <- rast(x=xy[,c('x','y','y')], type="xyz", crs=crs(x), extent=ext(x)) |> project(x)
names(lon) <- 'lon'
names(lat) <- 'lat'
Tcl <- min(br.tl)

names(Tclx)<-'Tclx'
writeRaster(Tclx, 'clim1990/Tclx.tif')

XtremLow.rast <- function(Tcl, elev){
  Tcl <- min(Tcl)
  xyr <- rast(nrows=180, ncols=360, nlyrs=1, crs=crs('EPSG:4326'), extent=ext(-180,180,-90,90), vals=1)
  xy <- terra::as.data.frame(x=xyr, xy=TRUE)
  lon <- rast(x=xy[,c('x','y','x')], type="xyz", crs=crs(xyr), extent=ext(xyr)) 
  lat <- rast(x=xy[,c('x','y','y')], type="xyz", crs=crs(xyr), extent=ext(xyr))
  names(lon) <- 'lon'
  names(lat) <- 'lat'
  
  pacificsouth <- 1/((((lat - -22.7)/13)^2 + ((lon - -82.3)/14)^2)^2+1)
  amazon2 <- 1/((((lat - -10.2)/5)^2 + ((lon - -59.9)/10)^2)^2+1)
  amazon1 <- 1/((((lat - -2.8)/14)^2 + ((lon - -61.3)/19)^2)^2+1)
  pacificcent <- 1/((((lat - 4.1)/21)^2 + ((lon - -122.4)/41)^2)^2+1)
  mexico <- 1/((((lat - 26)/6)^2 + ((lon - -98.4)/12)^2)^2+1)
  florida <- 1/((((lat - 27.5)/4)^2 + ((lon - -81.1)/8)^2)^2+1)
  pacificnorth <- 1/((((lat - 32.9)/26)^2 + ((lon - -145)/27)^2)^2+1)
  oklahoma <- 1/((((lat - 33.6)/4)^2 + ((lon - -98.4)/8)^2)^2+1)
  arizona <- 1/((((lat - 34)/12)^2 + ((lon - -113.1)/8)^2)^2+1)
  atlantic <- 1/((((lat - 34)/15)^2 + ((lon - -60.7)/19)^2)^2+1)
  himalayas <- 1/((((lat - 35.3)/6)^2 + ((lon - 91.3)/13)^2)^2+1)
  kentucky <- 1/((((lat - 38.5)/3)^2 + ((lon - -87.6)/9)^2)^2+1)
  detroit <- 1/((((lat - 41.8)/3)^2 + ((lon - -82.6)/4)^2)^2+1)
  ontario <- 1/((((lat - 44.6)/2)^2 + ((lon - -79.2)/6)^2)^2+1)
  montana <- 1/((((lat - 45.4)/5)^2 + ((lon - -111.8)/10)^2)^2+1)
  minn <- 1/((((lat - 47.6)/6)^2 + ((lon - -92.6)/12)^2)^2+1)
  hudson <- 1/((((lat - 60)/7)^2 + ((lon - -87)/34)^2)^2+1)
  siberia <- 1/((((lat - 61.2)/20)^2 + ((lon - 105.7)/39)^2)^2+1)
  california <- 1/((((lat - 34.8)/9)^2 + ((lon - -128.2)/9)^2)^2+1)
  washington <- 1/((((lat - 46)/5)^2 + ((lon - -126.6)/5)^2)^2+1)
  colorado <- 1/((((lat - 38.3)/2)^2 + ((lon - -108.8)/3)^2)^2+1)
  hawaii <- 1/((((lat - 21.3)/7)^2 + ((lon - -157.5)/11)^2)^2+1)
  chess <- 1/((((lat - 37)/3)^2 + ((lon - -74)/3)^2)^2+1)
  
  latlon <-	-9.171	+
    lat *	-0.04149	+
    pacificsouth *	-1.792	+
    amazon2 *	2.573	+
    amazon1 *	-1.014	+
    pacificcent *	-0.749	+
    mexico *	-0.8227	+
    florida *	-3.557	+
    pacificnorth *	-1.246	+
    oklahoma *	0.1758	+
    arizona *	2.605	+
    chess *	0.8347	+
    atlantic *	0.2967	+
    himalayas *	-1.814	+
    kentucky *	-2.644	+
    detroit *	0	+
    ontario *	-2.314	+
    montana *	-4.415	+
    minn *	1.136	+
    hudson *	-5.154	+
    siberia *	-3.797	+
    california *	4.48	+
    washington *	3.597	+
    colorado *	1.458	+
    hawaii *	6.673
  
  lat <- project(lat, elev)
  
  latlon <- project(Tclx0, elev)
  
  Tclx <-	Tcl *	1.202	+
    latlon +
    lat *	-0.04149	+
    elev *	0.0008691	+
    lat * elev *	-0.00002455
  names(Tclx) <- 'Tclx'
  return(Tclx)}


Tclx <- XtremLow.rast(Tcl,elev)
plot(Tclx)
writeRaster(Tclx, "clim1990/Tclx.tif", overwrite=T)


p <- sum(br.p)
names(p) <- 'p'
writeRaster(p, "clim1990/p.tif", overwrite=T)



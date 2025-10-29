library(terra)
library(sf)
library(climatools)
library(gstat)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#build covariate rasters for climate ----
elev <- rast("C:/scripts/processclimategrids/output/Elev1km.tif")
elev <- ifel(is.na(elev),0,elev)
names(elev)<-'elev'
w50 <- rast('C:/scripts/vegmap/global/w50.tif')
w500 <- rast('C:/scripts/vegmap/global/w500.tif')
w5000 <- rast('C:/scripts/vegmap/global/w5000.tif')
br <- c(elev,w50,w500,w5000)

br2 <- aggregate(br, fact=20, fun="mean", rm.na=T)
x <- (-1800:1800)/10; y <- (-900:900)/10
xy <- merge(x, y)
lat <- rast(data.frame(x=xy$x,y=xy$y, z=xy$y), type="xyz", crs=crs(br2))
xy <- xy |> mutate(coslon = cos(x/360*2*pi), sinlon = sin(x/360*2*pi))
coslon <- rast(data.frame(x=xy$x,y=xy$y, z=xy$coslon), type="xyz", crs=crs(br2))
sinlon <- rast(data.frame(x=xy$x,y=xy$y, z=xy$sinlon), type="xyz", crs=crs(br2))
lat <- project(lat, br2)
coslon <- project(coslon, br2)
sinlon <- project(sinlon, br2)
names(lat) <- 'lat';names(coslon) <- 'coslon';names(sinlon) <- 'sinlon';
br2 <- c(br2,lat,coslon,sinlon)
writeRaster(br2,'ghcn/br.tif', overwrite=T)
# writeRaster(br2,'ghcn/br2.tif', overwrite=T)




#start loading data to play with
br2 <- rast('ghcn/br.tif')
#load point data excluding potentially bad records
tsdata <- readRDS('ghcn/tsdata.RDS') |> subset(!is.na(YEAR) & !ID %in% c('CA002402051','CA002300551')) 
#create weights for sampling equally distributed records on a global basis
tsdata <- tsdata |> mutate(elevzone = floor(((elev/5000)+0.1)^0.5*5), 
                           lzone = floor((LONGITUDE/180+1)*5)*10+floor((LATITUDE/90+1)*5)*10)
tsdata <- tsdata |> group_by(elevzone, lzone, YEAR) |> mutate(wts = 100/(length(NAME)+1)) |> ungroup()


br2$YEAR <- 2000
library(gam)
library(ranger)
library(gstat)
tsdata <- tsdata |>mutate(x=LONGITUDE,y=LATITUDE, lat=y, coslon = cos(x/360*2*pi), sinlon = sin(x/360*2*pi))


tsdata <- st_as_sf(tsdata, coords = c(x='LONGITUDE', y='LATITUDE'), crs=crs(br2))
#generate equally distributed rareified training layer for global model
tdat <- tsdata[sample(1:nrow(tsdata), size=1000, prob=tsdata$wts), ]




### test models on rasters
smoothvars <- c("lat","sinlon","coslon","elev","w50","w500","w5000","YEAR")
train <- tdat |> mutate(z=t01) |> subset(!is.na(z))
train <- train |> subset(!is.na(z))
formular.gam <- as.formula(paste(paste("z",paste(paste("s(",smoothvars,",4)", collapse = " + ", sep = ""),""), sep = " ~ "), "+lat*coslon*sinlon*elev*YEAR"
))


gm <- gam(formular.gam,
          family='gaussian',
          data=train, rm.na=T)
summary(gm)

rf <- ranger(y~lat+sinlon+coslon+elev+w50+w500+w5000+YEAR,
          data=st_drop_geometry(train))


pr <- predict(br2, gm, na.rm=T, type = "response")
pr <- predict(br2, rf, na.rm=T);  

plot(pr)

#how to interpolate
train1 <- st_drop_geometry(train) |> subset(!is.na(y) & !is.na(x))
gs <- gstat(formula=t01~1, locations=~x+y, data=train1)
idw <- interpolate(br2, gs, debug.level=0)
plot(idw)
#thin spline interpolation
library(fields)
xy <- df2[,c('x','y')] |> st_drop_geometry() |> as.data.frame()
v <- df2$res1
elev = df2$elev
xyz <- cbind(xy, elev)

timeA <- Sys.time()
#elevation covary
tps <- Tps(xyz, v, lon.lat = TRUE)
res1 <- interpolate(c(br3$elev), tps, xyOnly=FALSE)#
Sys.time() - timeA
# timeA <- Sys.time()
# plot(res1)
#xy only
tps <- Tps(xy, v, lon.lat = TRUE)
res1 <- interpolate(rast(br3$elev), tps)#, xyOnly=TRUE
Sys.time() - timeA
plot(res1)

#Also tried MARS with earth package, but it only does what gam does but with broken lines, and does not interpolate.
#Kriging too slow (10 hours and never completing despite small dataset and coarse rasters)
# gs <- gstat(formula=t01~1, locations=~x+y, data=train1)
# v <- variogram(gs, width=2)
# fve <- fit.variogram(v, vgm(85, "Exp", 75, 20))
# fve
# plot(variogramLine(fve, 150), type='l', ylim=c(0,300))
# points(v[,2:3], pch=20, col='red')
# 
# 
# k <- gstat(formula=t01~1, locations=~x+y, data=train1, model=fve)
# # predicted values
# kp <- interpolate(br2, k, debug.level=0)
# plot(kp)



#Interpolate missing values for 1961-2010 segmented geography----
library(terra)
library(sf)
library(climatools)
library(gam)
library(ranger)
library(fields)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#load reduced resolution covariate rasters
br2 <- rast('ghcn/br.tif')
#create raster representing one year for use in a raster model
yr = 1990
br2$YEAR <- yr
#narrow raster model to that of specific geography
newext <- ext(-95, -75, 30, 40)
br3 <- crop(br2, newext)
#load point data and create covariates, potential weights for evenly distrubuted sampling, and covert to a sf spatial data frame.
tsdata <- readRDS('ghcn/tsdata.RDS') |> subset(!is.na(YEAR) & !ID %in% c('CA002402051','CA002300551','CA006092920'))
tsdata <- tsdata |> mutate(elevzone = floor(((elev/5000)+0.1)^0.5*5), 
                           lzone = floor((LONGITUDE/180+1)*5)*10+floor((LATITUDE/90+1)*5)*10)
tsdata <- tsdata |> group_by(elevzone, lzone, YEAR) |> mutate(wts = 100/(length(NAME)+1)) |> ungroup()
tsdata <- tsdata |>mutate(x=LONGITUDE,y=LATITUDE, lat=y, coslon = cos(x/360*2*pi), sinlon = sin(x/360*2*pi))
tsdata <- st_as_sf(tsdata, coords = c(x='LONGITUDE', y='LATITUDE'), crs=crs(br2))

#narrow all records down to just 1961-2010
yrs <- c(1700,2030)
df <- tsdata |> subset(YEAR >= yrs[1] & YEAR <= yrs[2]) |> mutate(z=NA,pr1=NA,res1=NA,pr2=NA,newz=NA)

#create missing years for all records.
dfs <- df |> st_drop_geometry()|> group_by(ID,NAME, elev, x,y) |> 
  summarise(YEARS=length(YEAR), yfirst=min(YEAR),ylast=max(YEAR), meanT01 = mean(t01, na.rm=T),meanT07 = mean(t07, na.rm=T))


# ggplot()+
#   geom_density(data=df, aes(x=YEAR))+
#   scale_x_continuous(breaks = seq(1700,2030, 20))
ys <- seq(-90,90,10)
xs <- seq(-180,180,15)
yrs <- seq(1961,2010,1)
zvars <- c('t01','t02','t03','t04','t05','t06','t07','t08','t09','t10','t11','t12')

yi=14
xi=8
zi=1
yri=30

#set dependent variable
for(zi in 1:12){
zvar <- zvars[zi]
z0 = df[,zvar] |> st_drop_geometry() |> as.data.frame() 
z0 <- z0[,1]
df[,] <- df[,] |> mutate(z = z0)
#index to apply values to a given geography
for(yi in 1:(length(ys)-1)){
  for(yi in 1:(length(ys)-1)){
predext <- ext(xs[xi ], xs[xi+1], ys[yi], ys[yi+1])
trainext <- ext(xs[xi ]-7.5, xs[xi+1]+7.5, ys[yi]-5, ys[yi+1]+5)
index1 <- df$x >= trainext[1] & df$x <= trainext[2] & df$y >= trainext[3] & df$y <= trainext[4]


#index for training gam with non missing z
index2 <- !is.na(df$z) & index1

#gam
train.gam <- df[index2,]
m <- gam(z ~ s(lat)+s(elev)+w50+w500+w5000+s(YEAR,4)+s(coslon)+s(sinlon), data = train.gam)
df[index1,] <- df[index1,]  |> mutate(pr1 = predict(m, df[index1,]), res1 = z-pr1)

#need to focus on one year at a time
for(yri in 1:length(yrs)){
yr=yrs[yri]
#index for training tps on residuals for specific year
index3 <- df$YEAR == yr & index2

#index for interpolating residuals only on missing records for focal extent
index4 <- df$YEAR == yr & index1 & is.na(df$z) & 
  df$x >= predext[1] & df$x <= predext[2] & df$y >= predext[3] & df$y <= predext[4]


train.tps <- df[index3,] |> st_drop_geometry() |> as.data.frame()
xyz <- train.tps[,c('x','y','elev')] 
v <- train.tps$res1
tps <- Tps(xyz, v, lon.lat = TRUE)
tps.unk <- df[index4,c('x','y','elev')] |> st_drop_geometry() |> as.data.frame()
pr0 <- predict(object=tps, x=tps.unk)[,1]
df[index4,]$pr2 <- pr0
df[index4,] <- df[index4,] |> mutate(newz = pr2+pr1)

#end year
}

#end geography
  }}
#end zvar
}
#----------------



summary(m)
pr <-predict(br3, m, na.rm=T)
# gg <- vect(df)
plot(pr)
# points(gg)
df2 <- df |> mutate(res1 = t01-pr1) |> subset(!is.na(res1))



prres1 <- res1+pr
plot(prres1)


names(prres1) <- paste0('y',yr)
assign(paste0('y',yr), prres1)

plot(y2000-y1900)

woelev <- y2000-y1900

welev <- y2000-y1900
plot(welev)
plot(woelev)


# plot(st_geometry(df))
library(ggplot2)
ggplot()+
  geom_smooth(data=subset(df, grepl('MI GRAND RAP',NAME)), aes(x=YEAR, y=t01))+
  geom_point(data=subset(df, grepl('MI GRAND RAP',NAME)), aes(x=YEAR, y=t01))+
  geom_point(data=subset(df, grepl('MI GRAND RAP',NAME)), aes(x=YEAR, y=pr1), color='red')



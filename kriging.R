library(terra)
library(sf)
library(climatools)
library(gstat)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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




#start
br2 <- rast('ghcn/br.tif')
tsdata <- readRDS('ghcn/tsdata.RDS') |> subset(!is.na(YEAR) & !ID %in% c('CA002402051','CA002300551'))
tsdata <- tsdata |> mutate(elevzone = floor(((elev/5000)+0.1)^0.5*5), 
                           lzone = floor((LONGITUDE/180+1)*5)*10+floor((LATITUDE/90+1)*5)*10)
tsdata <- tsdata |> group_by(elevzone, lzone, YEAR) |> mutate(wts = 100/(length(NAME)+1)) |> ungroup()


br2$YEAR <- 2000
library(gam)
library(ranger)
library(gstat)
tsdata <- tsdata |>mutate(x=LONGITUDE,y=LATITUDE, lat=y, coslon = cos(x/360*2*pi), sinlon = sin(x/360*2*pi))


tsdata <- st_as_sf(tsdata, coords = c(x='LONGITUDE', y='LATITUDE'), crs=crs(br2))
tdat <- tsdata[sample(1:nrow(tsdata), size=1000, prob=tsdata$wts), ]




###
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


train1 <- st_drop_geometry(train) |> subset(!is.na(y) & !is.na(x))
gs <- gstat(formula=t01~1, locations=~x+y, data=train1)
idw <- interpolate(br2, gs, debug.level=0)
plot(idw)
#kriging too slow (10 hours and never completing despite small dataset and coarse rasters)
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

library(terra)

# example data
xy <- train[,c('x','y')] |> st_drop_geometry()
v <- train$t01
z = train$elev
m = train$w500
# Thin plate spline model
library(fields)
xyzm <- cbind(xy, z,m)
tps2 <- Tps(xyzm, v)
p2 <- interpolate(c(br2$elev,br2$w500), tps2, xyOnly=FALSE)
plot(p2)

#segmentation 

train2 <- tsdata |> subset(x >-180 & x< -50  & y > 10 & y < 80 & !is.na(t01))|> mutate(w5550=(1-w50)*(1-w500)*(1-w5000))
train2 <- train2[sample(1:nrow(train2), size=2000, prob=train2$wts, replace = T),]
br3 <- crop(br2, ext(-180, -50, 10, 80)) 

gm <- gam(t01 ~ lat+elev+w50+w500+w5000+YEAR+coslon+sinlon, data=tsdata)
summary(gm)
pr <-predict(br3, gm, na.rm=T)
plot(pr)
points(st_geometry(train2))


oldrec <- tsdata |> subset(YEAR <= 1950)
plot(br2$elev)+ points(st_geometry(oldrec))
train2 <- df2
# example data
xy <- train2[,c('res1','x','y','elev')] |> st_drop_geometry() |> unique()
v <- xy$res1
xy <- xy[,2:4]
# z = train2$elev
# m = train2$YEAR
# Thin plate spline model
library(fields)
# xyzm <- cbind(xy, z,m)
tps2 <- Tps(xy, v)
p2 <- interpolate(c(br3$elev), tps2, xyOnly=FALSE)
plot(p2)


#segmentation2----
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

tsdata <- readRDS('ghcn/tsdata.RDS') |> subset(!is.na(YEAR) & !ID %in% c('CA002402051','CA002300551'))
tsdata <- tsdata |> mutate(elevzone = floor(((elev/5000)+0.1)^0.5*5), 
                           lzone = floor((LONGITUDE/180+1)*5)*10+floor((LATITUDE/90+1)*5)*10)
tsdata <- tsdata |> group_by(elevzone, lzone, YEAR) |> mutate(wts = 100/(length(NAME)+1)) |> ungroup()
tsdata <- tsdata |>mutate(x=LONGITUDE,y=LATITUDE, lat=y, coslon = cos(x/360*2*pi), sinlon = sin(x/360*2*pi))
tsdata <- st_as_sf(tsdata, coords = c(x='LONGITUDE', y='LATITUDE'), crs=crs(br2))





# ggplot()+
#   geom_density(data=df, aes(x=YEAR))+
#   scale_x_continuous(breaks = seq(1700,2030, 20))




yrs <- c(1961,2010)
newext <- ext(-95, -75, 30, 40)
zvar <- 't01'
#narrow all records down to just 1961-2010
df <- tsdata |> subset(YEAR >= yrs[1] & YEAR <= yrs[2]) |> mutate(z=NA,pr1=NA,res1=NA,pr2=NA)

#index to apply values to a given geography
index1 <- df$x >= newext[1] & df$x <= newext[2] & df$y >= newext[3] & df$y <= newext[4]


#set dependent variable
z0 = df[index1,zvar] |> st_drop_geometry() |> as.data.frame() 
z0 <- z0[,1]
df[index1,] <- df[index1,] |> mutate(z = z0)

#index for training gam with non missing z
index2 <- !is.na(df$z) & index1

#index for training tps on residuals for specific year
index3 <- df$YEAR == yr & index2

#index for interpolating residuals only on missing records
index4 <- df$YEAR == yr & index1 & is.na(df$z) 

#gam model
train.gam <- df[index2,]
m <- gam(z ~ s(lat)+s(elev)+w50+w500+w5000+s(YEAR,4)+s(coslon)+s(sinlon), data = train.gam)
df[index1,] <- df[index1,]  |> mutate(pr1 = predict(m, df[index1,]), res1 = z-pr1)

#need to focus on one year at a time
yr=1990
train.tps <- df[index2,]
xyz <- train.tps[,c('x','y','elev')] |> st_drop_geometry() |> as.data.frame()
v <- train.tps$res1
tps <- Tps(xyz, v, lon.lat = TRUE)
tps.unk <- df[df$YEAR %in% yr,c('x','y','elev')] |> st_drop_geometry() |> as.data.frame() #is.na(df$z) & 
pr3 <- predict(object=tps, x=tps.unk)[,1]
df[ df$YEAR %in% yr,]$pr3 <- pr3 #is.na(df$z) &
df <- df |> mutate(newz = pr3+pr1)

summary(m)
pr <-predict(br3, m, na.rm=T)
# gg <- vect(df)
plot(pr)
# points(gg)
df2 <- df |> mutate(res1 = t01-pr1) |> subset(!is.na(res1))

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
# # plot(res1)
# #xy only
# tps <- Tps(xy, v, lon.lat = TRUE)
# res1 <- interpolate(rast(br3$elev), tps)#, xyOnly=TRUE
# Sys.time() - timeA
plot(res1)


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




library(earth)
train2 <- tsdata |> mutate(x=LONGITUDE,y=LATITUDE) |> subset(x >-180 & x< -130  & y > 55 & y < 80 & !is.na(t01))
train2 <- st_as_sf(train2, coords = c(x='LONGITUDE', y='LATITUDE'), crs=crs(br2))
br3 <- crop(br2, ext(-180, -130, 55, 80)) 
mars <- earth(t01 ~ lat+sinlon+coslon+elev+w50+w500+w5000+YEAR, degree=5, penalty=0, data=train2)
summary(mars)
pr <-predict(br3, mars, na.rm=T)
plot(pr)+points(st_geometry(train2))


train2 <- tsdata |> mutate(x=LONGITUDE,y=LATITUDE) |> subset(!is.na(t01))

mars <- earth(t01 ~ lat+sinlon+coslon+elev+w50+w500+w5000+YEAR, degree=5, data=train2)
summary(mars)
pr <-predict(br3, mars, na.rm=T)
plot(pr)
clat<- 43
clon<- -86

train2 <- train2 |> mutate(wt = 100/((((x-clon)*0.75)^2+(y-clat)^2)^0.5+3))

mars <- earth(t01 ~ lat+sinlon+coslon+elev+w50+w500+w5000+YEAR, degree=5, data=train2, weights=wt)
summary(mars)
pr <-predict(br3, mars, na.rm=T)
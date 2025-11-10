library(terra)
library(sf)
library(climatools)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# chelsa <- rast('C:/scripts/vegmap/chelsa2.1/chelsa.tif')
# names(chelsa)
stations.p <- read.fwf('ghcn/v2/v2.prcp.inv', widths = c(11,21,10,7,8,5), comment.char = "")
colnames(stations.p) <- c('ID','NAME','STATE','LATITUDE','LONGITUDE','STNELEV')
# write.csv(stations.p, 'ghcn/stations.p.csv', row.names = FALSE)
pdata <- read.fwf('ghcn/v2/v2.prcp/v2.prcp',
                    widths = c(11,1,4,
                               5,
                               5,
                               5,
                               5,
                               5,
                               5,
                               5,
                               5,
                               5,
                               5,
                               5,
                               5), fill=T)

colnames(pdata) <- 
  c('ID','FLAG','YEAR',
    'p01',
    'p02',
    'p03',
    'p04',
    'p05',
    'p06',
    'p07',
    'p08',
    'p09',
    'p10',
    'p11',
    'p12')


makena <- function(x){ifelse(x %in% -9999,NA,ifelse(x %in% -8888,0.2,x))}


pdata[,c(4:15)] <- sapply(pdata[,c(4:15)], FUN=makena )
pdata[,c(4:15)] <- pdata[,c(4:15)]/10
pdata <- pdata[,c('ID','YEAR',
                  'p01',
                  'p02',
                  'p03',
                  'p04',
                  'p05',
                  'p06',
                  'p07',
                  'p08',
                  'p09',
                  'p10',
                  'p11',
                  'p12')]
# saveRDS(pdata, 'ghcn/pdata.RDS')
#1981-2010 normals because many high Appalachian stations are missing from GHCN

nstations <- read.fwf('1981-2010/ghcnd-stations.txt', widths = c(11,1,8,1,9,1,6,1,2,1,30,1,3,1,3,1,5,1,13), comment.char = "")
colnames(nstations) <- c('ID','x1','LATITUDE','x2','LONGITUDE','x3','STNELEV','x4','STATE','x5','NAME','x6','GSNFLAG','x7','HCNFLAG','x8','WMOID','x9','METHOD')
keep <- c('ID','LATITUDE','LONGITUDE','STNELEV','NAME','STATE')
nstations <- nstations[,keep]
npdata <- read.fwf('1981-2010/mly-prcp-filled.txt', widths = c(11,1,4,1,
                                                                  5,2,
                                                                  5,2,
                                                                  5,2,
                                                                  5,2,
                                                                  5,2,
                                                                  5,2,
                                                                  5,2,
                                                                  5,2,
                                                                  5,2,
                                                                  5,2,
                                                                  5,2,
                                                                  5,2), fill=T)
colnames(npdata) <- 
  c('ID','FLAG','YEAR','FLAG',
    'p01','FLAG',
    'p02','FLAG',
    'p03','FLAG',
    'p04','FLAG',
    'p05','FLAG',
    'p06','FLAG',
    'p07','FLAG',
    'p08','FLAG',
    'p09','FLAG',
    'p10','FLAG',
    'p11','FLAG',
    'p12','FLAG')
npdata <- npdata[,c('ID','YEAR',
                    'p01',
                    'p02',
                    'p03',
                    'p04',
                    'p05',
                    'p06',
                    'p07',
                    'p08',
                    'p09',
                    'p10',
                    'p11',
                    'p12')]
makena <- function(x){ifelse(x %in% -9999,NA,x)}


npdata[,c(3:14)] <- sapply(npdata[,c(3:14)], FUN=makena )
npdata[,c(3:14)] <- npdata[,c(3:14)]/10
# npdata[,c(3:14)] <- npdata[,c(3:14)]*25.4
#combined records
stations.p <- stations.p |> rbind(nstations)
write.csv(stations.p, 'ghcn/stations.p.csv', row.names = FALSE)
pdata <- pdata |> rbind(npdata)
saveRDS(pdata, 'ghcn/pdata.all.RDS')
###############
library(terra)
library(sf)
library(climatools)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
elev <- rast('global/elev.tif')
wind <- rast('global/wind.tif')
br <- rast('ghcn/br.tif')
stations.p <- read.csv('ghcn/stations.p.csv')
stations.p <- stations.p |> mutate(y=LATITUDE,x=LONGITUDE) |> st_as_sf(coords = c(x='LONGITUDE',y='LATITUDE'), crs=st_crs(elev))
pdata <- readRDS('ghcn/pdata.all.RDS')
newelev <- extract(elev, vect(stations.p))
newwind <- extract(wind, project(vect(stations.p), wind))
newbr <- extract(br, project(vect(stations.p), br))
stations.p <- stations.p |> mutate(elev = newelev$elev)

# fst <- stations.p |> mutate(edif=((elev+1000)^0.5-(STNELEV+1000)^0.5)/((500+1000)^0.5-(0+1000)^0.5))
# fst <- fst |> left_join(pdata)

stations.p <- stations.p |> mutate(STNELEV = case_when(STNELEV %in% -999 ~ elev,
                                                       STNELEV == 0 ~ elev, 
                                                       TRUE ~ STNELEV))
stations.p <- stations.p |> mutate(elev = STNELEV,
                                   lat = y, 
                                   sinlon = sin(x/360*2*pi), 
                                   coslon = cos(x/360*2*pi),
                                   w50=newbr$w50,
                                   w500=newbr$w500,
                                   w5000=newbr$w5000,
                                   wind000=newwind$wind000,
                                   wind045=newwind$wind045,
                                   wind090=newwind$wind090,
                                   wind135=newwind$wind135,
                                   wind180=newwind$wind180,
                                   wind225=newwind$wind225,
                                   wind270=newwind$wind270,
                                   wind315=newwind$wind315)

pdata <- stations.p |> left_join(pdata)

#####
library(gam)
br2 <- rast('ghcn/br.tif')
elev2 <- rast('global/elev.tif') |> aggregate(fact=20)
br2$elev <- elev2
wind2 <-rast('ghcn/wind.tif')
br2 <- c(br2, wind2)
br2$YEAR <- 1990
pdata <- pdata |> mutate(elevzone = floor(((elev/5000)+0.1)^0.5*5), 
                      lzone = floor((x/180+1)*50)*10+floor((y/90+1)*5)*100) |> group_by(elevzone, lzone, YEAR) |> mutate(wts = 100/(length(NAME)+1)) |> ungroup()

newext <- ext(-90,-75, 30, 40)
br3 <- br2 |> crop(newext)
train0 <- pdata |> mutate(z = p01) |> subset(!is.na(z)) 
train2 <- train0 |> subset(x >= newext[1] & x <= newext[2] & y >= newext[3] & y <= newext[4])
train1 <- train0[sample(1:nrow(train0), size=500, prob=train0$wts, replace = TRUE),]
train2 <- train2[sample(1:nrow(train2), size=50000, prob=train2$wts, replace = TRUE),]
train <- rbind(train2)
trainC <- train |> mutate(z=log2(z+10))
gmC <- gam(
  z ~ s(lat) + s(sinlon) + s(coslon)+
    s(elev)+(w500) +(w5000) +
     s(YEAR) + 
    s(wind000)+ 
    s(wind045)+  
    s(wind090)+  
    s(wind135)+  
    s(wind180)+  
    s(wind225)+  
    s(wind270)+  
    s(wind315) 
  , data=trainC, family = 'gaussian')

summary(gmC)
prC <- predict(br3, gmC, na.rm=T, type = "response")
# prC <- 2^prC-10
plot((prC))
points(st_geometry(train), cex=0.5)


plot(2^prC-10)

trainC <- trainC |> mutate(pr1 = predict(gmC, train), res1 = z-pr1)
library(fields)
xyz <- trainC[,c('x','y','elev')] |> st_drop_geometry() 
v <- trainC$res1
tps <- Tps(xyz, v, lon.lat = TRUE)
res1 <- interpolate(c(br3$elev), tps, xyOnly=F)#

# tps <- Tps(xyz[,c('x','y')], v, lon.lat = TRUE)
# res1 <- interpolate(rast(br3$elev), tps, xyOnly=T)#
plot(res1)
newgrid <- res1+prC
plot(newgrid)
# res1idw <- interpIDW(br3$elev, vect(trainC[,c('x','y','res1')]), field='res1', radius=5)#
# plot(res1idw)
# plot(res1idw+prC)
# 
# 
# test <- pdata |> subset(ID %in% c('USW00094860', '42572635000'))
# test <- test |> st_drop_geometry()|> group_by(ID, NAME) |> summarise(across(all_of(c('p01',
#                                                                           'p02',
#                                                                           'p03',
#                                                                           'p04',
#                                                                           'p05',
#                                                                           'p06',
#                                                                           'p07',
#                                                                           'p08',
#                                                                           'p09',
#                                                                           'p10',
#                                                                           'p11',
#                                                                           'p12')),mean, na.rm=T))
# apply(test[,c('p01',
#         'p02',
#         'p03',
#         'p04',
#         'p05',
#         'p06',
#         'p07',
#         'p08',
#         'p09',
#         'p10',
#         'p11',
#         'p12')],1,sum)
# 2467.8301/883.6245
# 
# trainA <- train |> subset(z > 0)
# trainB <- train |> mutate(z = ifelse(z <= 0,0,1))
# gmA <- gam(
#   z ~ s(lat) + s(sinlon) + s(coslon) + s(w50) +  s(elev):s(w500) + 
#     s(elev):s(w5000) + s(lat):s(YEAR) + s(lat):s(wind000)+ s(lat):s(wind045)+ s(lat):s(wind090)+ s(lat):s(wind135)+ s(lat):s(wind180)+ s(lat):s(wind225)+ s(lat):s(wind270)+ s(lat):s(wind315)
#   , data=trainA, family = 'Gamma')
# gmB <- gam(
#   z ~ s(lat) + s(sinlon) + s(coslon) + s(w50) +  s(elev):s(w500) + 
#     s(elev):s(w5000) + s(lat):s(YEAR) + s(lat):s(wind000)+ s(lat):s(wind045)+ s(lat):s(wind090)+ s(lat):s(wind135)+ s(lat):s(wind180)+ s(lat):s(wind225)+ s(lat):s(wind270)+ s(lat):s(wind315)
#   , data=trainB, family = 'binomial')
# 
# summary(gmA)
# summary(gmB)
# prA <- predict(br3, gmA, na.rm=T, type = "response")
# prB <- predict(br3, gmB, na.rm=T, type = "response")
# pr<-prA+prB
# plot(log2(pr))
# points(st_geometry(train))

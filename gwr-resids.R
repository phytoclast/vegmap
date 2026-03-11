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
  x <- ifelse(x < 0,0,x)
  return(x)}
alogtr <- function(x){
  x <- 10^x-0.01
  x <- ifelse(x < 0,0,x)
  return(x)}

# elev <- rast('global/elev.tif')
# relev <- rast('global/relev2.tif')
elevmin <- rast('global/elevmin.tif')
# wind <- rast('global/wind.tif')
# water <- rast('global/water.tif')
# br <- rast('ghcn/br2.tif')
# wind1 <- project(wind, br)
# relev1 <- project(relev, br)
# br1 <- c(br,relev1,wind1)
# br1 <- br1[[-1]]
# elev1 <- project(elev, br)
# br1 <- c(elev1,br1)
# names(br1)
# writeRaster(br1,'global/br5000.tif')
br1 <- rast('global/br5000.tif')
belts <- rast('global/belts.tif')
bsns <- rast('ghcn/bsns.tif')
pchelsa <- rast('global/pchelsa.tif')
pchelsa <- pchelsa |> project(br1)
tchelsa <- rast('global/tchelsa.tif')
tchelsa <- tchelsa |> project(br1)
# belts <- belts |> project(br1)
br1 <- c(br1,belts, pchelsa, tchelsa)
br1$tropical <- br1$monsoonE+br1$monsoonW+br1$tradeN+br1$tradeS
psummer <- rast('global/psummer.tif')
trsummer <- rast('global/trsummer.tif')
tsummer <- rast('global/tsummer.tif')
pwinter <- rast('global/pwinter.tif')
trwinter <- rast('global/trwinter.tif')
twinter <- rast('global/twinter.tif')
basemap <- c(psummer,trsummer,tsummer,pwinter,trwinter,twinter, bsns)
br1 <- c(br1,basemap)
elev1 <- br1[[c('elev','windmean','wind090','wind270')]] |> aggregate(3)
pdata <- read.csv('ghcn/filled/pnorm1990.csv')
tdata <- read.csv('ghcn/filled/tnorm1990.csv')
trdata <- read.csv('ghcn/filled/trnorm1990.csv')
stations.p <- readRDS('ghcn/stations.p2.RDS')
stations.p[stations.p$ID %in% 'NLE00109220',]$elev <- 0
stations.t <- readRDS('ghcn/stations.t2.RDS')
stations.tr <- readRDS('ghcn/stations.tr2.RDS')
stations.p <- stations.p |> cbind(elevmin=extract(elevmin,vect(stations.p))$elevmin)
stations.t <- stations.t |> cbind(elevmin=extract(elevmin,vect(stations.t))$elevmin) 
stations.tr <- stations.tr |> cbind(elevmin=extract(elevmin,vect(stations.tr))$elevmin)
stations.p <- stations.p |> cbind(extract(pchelsa,vect(stations.p)))
stations.t <- stations.t |> cbind(extract(tchelsa,vect(stations.t)))
stations.tr <- stations.tr |> cbind(extract(tchelsa,vect(stations.tr)))
stations.p <- stations.p |> cbind(extract(belts,vect(stations.p)))
stations.t <- stations.t |> cbind(extract(belts,vect(stations.t)))
stations.tr <- stations.tr |> cbind(extract(belts,vect(stations.tr)))

stations.tr <- stations.tr |> cbind(extract(basemap,vect(stations.tr)))
stations.p <- stations.p |> cbind(extract(basemap,vect(stations.p)))
stations.t <- stations.t |> cbind(extract(basemap,vect(stations.t)))

stations.p <- stations.p |> mutate(tropical = monsoonW+monsoonE+tradeN+tradeS)
stations.t <- stations.t |> mutate(tropical = monsoonW+monsoonE+tradeN+tradeS)
stations.tr <- stations.tr |> mutate(tropical = monsoonW+monsoonE+tradeN+tradeS)
stations.p <- stations.p |> mutate(relev=elev-elevmin, relev=relev/(relev+500), relev=ifelse(relev>1,1, ifelse(relev<0,0,relev)))
stations.t <- stations.t |> mutate(relev=elev-elevmin, relev=relev/(relev+500), relev=ifelse(relev>1,1, ifelse(relev<0,0,relev)))
stations.tr <- stations.tr |> mutate(relev=elev-elevmin, relev=relev/(relev+500), relev=ifelse(relev>1,1, ifelse(relev<0,0,relev)))
pdata <- stations.p |> left_join(subset(pdata, select=-elev)) |> subset(!is.na(p01))
tdata <- stations.t |> left_join(subset(tdata, select=-elev)) |> subset(!is.na(t01))
trdata <- stations.tr |> left_join(subset(trdata, select=-elev)) |> subset(!is.na(tr01))

tdatcols <- c("t01","t02","t03","t04","t05","t06","t07","t08","t09","t10","t11","t12")
trdatcols <- c("tr01","tr02","tr03","tr04","tr05","tr06","tr07","tr08","tr09","tr10","tr11","tr12")
pdatcols <- c("p01","p02","p03","p04","p05","p06","p07","p08","p09","p10","p11","p12")
trdata. <- trdata |> mutate(across(all_of(trdatcols), logtr))
pdata. <- pdata |> mutate(across(all_of(pdatcols), logp))
pdata. <- pdata. |> mutate(geotile = paste('ll',floor(x/5)*100,floor(y/5)), elzone = floor(((elev/500)+0.1)^1*1))|> group_by(geotile, elzone) |> mutate(wts0=100/length(ID)) |> ungroup()
trdata. <- trdata. |> mutate(geotile = paste('ll',floor(x/5)*100,floor(y/5)), elzone = floor(((elev/500)+0.1)^1*1))|> group_by(geotile, elzone) |> mutate(wts0=100/length(ID)) |> ungroup()
tdata <- tdata |> mutate(geotile = paste('ll',floor(x/5)*100,floor(y/5)), elzone = floor(((elev/500)+0.1)^1*1))|> group_by(geotile, elzone) |> mutate(wts0=100/length(ID)) |> ungroup()

#identify permanent points
pdata. <- pdata. |> 
  mutate(geotile2 = paste('ll',floor(x/15)*100,floor(y/15)), wtriple = (w50/10+w500+w5000/10)/1.2) |> group_by(geotile2) |> mutate(minwater = min(wtriple), maxwater = max(wtriple), mexelev = max(elev)) |>  ungroup() |>   mutate(keep = ifelse(wtriple == minwater | wtriple == maxwater| elev == mexelev, 1,0), minwater=NULL, maxwater=NULL, maxelev=NULL, geotile2=NULL)

trdata. <- trdata. |>
  mutate(geotile2 = paste('ll',floor(x/15)*100,floor(y/15)), wtriple = (w50/10+w500+w5000/10)/1.2) |> group_by(geotile2) |> mutate(minwater = min(wtriple), maxwater = max(wtriple), mexelev = max(elev)) |>  ungroup() |>   mutate(keep = ifelse(wtriple == minwater | wtriple == maxwater| elev == mexelev, 1,0), minwater=NULL, maxwater=NULL, maxelev=NULL, geotile2=NULL)

tdata <- tdata |>
  mutate(geotile2 = paste('ll',floor(x/15)*100,floor(y/15)), wtriple = (w50/10+w500+w5000/10)/1.2) |> group_by(geotile2) |> mutate(minwater = min(wtriple), maxwater = max(wtriple), mexelev = max(elev)) |>  ungroup() |>   mutate(keep = ifelse(wtriple == minwater | wtriple == maxwater| elev == mexelev, 1,0), minwater=NULL, maxwater=NULL, maxelev=NULL, geotile2=NULL)



pseudopoints <- st_read('ghcn/dummy.shp') 
pseudopoints <- pseudopoints |> cbind(extract(br1, vect(pseudopoints))) 
pseudopoints <- pseudopoints |> mutate(x=st_coordinates(pseudopoints)[,1],y=st_coordinates(pseudopoints)[,2],z=NA, wts0=0.1, keep=1) 
pseudopoints <- subset(pseudopoints, y < 84)



#######################
library(GWmodel)
ex <- c(-180, -10, 0, 90)
# ex <- c(-180, -10, -90, 20)

parm = 'p'
mo = 1
mos <- c("01","02","03","04","05","06","07","08","09","10","11","12")

br2 <- crop(br1, y=ext(ex))
br10 <- br2|> aggregate(10)
newpts <- br10$elev |> as.points()
plot(br2$elev)

for(mo in c(1:12)){#mo=8
  
  nameofpar <- paste0(parm,mos[mo])
  filnam <- paste0('clim1990/northamerica.',nameofpar,'.tif')
  
  if(parm %in% 't'){
    globl <- tdata |> subset(!ID %in% c('ACW00011604','CA002402051', 'CA002402332','CA002300551'))
    globl$z <- globl |> select(tdatcols[mo]) |> st_drop_geometry() |> as.vector() |> unlist()
    globl <- subset(globl, !is.na(tchelsa1))
    # globl <- globl |> subset(!is.na(z))
  }
  if(parm %in% 'tr'){
    globl <- trdata. |> subset(!ID %in% c('63512160000', '63512120000', '63512510000', '64112650000', '63512135000', '63512200000', '63512205000', '63512330000', '63512375000', '63512105000', '63512115000', '63512100000', '63512280000', '63512295000', '63512385000', '63512497000', '63512566000', '63512424000', '63512185000', '63512235000', '63512400000', '63512272000', '63512300000', '63512695000', '63512600000', '63512585000', '63512215000', '63512210000', '63512125000', '63512550000', '63512195000', '63512345000', '63512690000', '63512360000', '63512495000', '63512625000', '63512575000', '63512465000', '63512399000', '63512270000', '63512285000', '63512469000', '63512520000', '63512560000', '63512250000', '63512415000', '63512580000', '63512570000', '63512418000', '63512595000', '63512660000', '63512230000', '63512310000', '63512500000', '21135229000', '22223891000', '22229282000', '63826781000'))
    globl$z <- globl |> select(trdatcols[mo]) |> st_drop_geometry() |> as.vector() |> unlist()
    globl <- subset(globl, !is.na(trchelsa1))
    # globl <- globl |> subset(!is.na(z))
    # globl <- subset(globl, !(x > 10 & x < 30 & y > 45 & y < 60 & tr01 > 1.1))
  }
  if(parm %in% 'p'){
    globl <- pdata.
    globl$z <- globl |> select(pdatcols[mo]) |> st_drop_geometry() |> as.vector() |> unlist()
    globl <- subset(globl, !is.na(pchelsa1))
    # globl <- globl |> subset(!is.na(z))
  }
  comcol <- intersect(colnames(globl),colnames(pseudopoints))
  pspts <- pseudopoints[,comcol]
  globl <- globl[,comcol] |> rbind(pspts)
  
  
  
  if(parm %in% c('t')){
    globm = gam(z~s(coslon)+s(sinlon)+s(lat)+
                  relev+
                  elev+
                  elev:w5000+
                  tchelsa1+tchelsa2+tchelsa3+tchelsa4+
                  (w5000)+(w500)+(w50)
                ,data=globl, weights = globl$wts0)
  }else if(parm %in% c('tr')){
    globm = gam(z~s(coslon)+s(sinlon)+s(lat)+
                  relev+
                  elev+
                  elev:w5000+
                  trchelsa1+trchelsa2+trchelsa3+trchelsa4+
                  (w5000)+(w500)+(w50)
                ,data=globl, weights = globl$wts0)
  }else{
    globm = gam(z~s(coslon)+s(sinlon)+s(lat)+
                  # relev+
                  # elev+
                  # elev:w5000+
                  pchelsa1+pchelsa2+pchelsa3+pchelsa4#+
                  # windmax+windmin+windmean+
                  # (w500)+(w5000)+
                  # wind270:westerlyS+
                  # wind270:westerlyN+
                  # wind090:tradeN+
                  # wind090:tradeS+
                  # wind180:monsoonW+
                  # wind180:monsoonE+
                  # wind045:tradeN+
                  # wind135:tradeS+
                  # wind315:westerlyS+
                  # wind225:westerlyN+
                  # wind225:monsoonW+
                  # wind135:monsoonE+
                  # wind180:polarN+
                  # wind000:polarS
                ,data=globl, weights = globl$wts0)
  }
  
  smry <- summary(globm)
  smry$aic
  train <- globl |> subset(x >= ex[1] & x <= ex[2] & y >= ex[3] & y <= ex[4]) #|> st_drop_geometry
  train <- train |> mutate(pred = predict(globm, train, na.rm=T, type = "response"), z=ifelse(is.na(z),pred,z), resid = z-pred) |> subset(!is.na(z))
    
  
  trainpts <- vect(train[,NULL])
  combpts <- rbind(newpts,trainpts)
  # summary(globm)
  if(parm %in% c('t')){
    gw <- gwr.basic(resid ~ lat+coslon+sinlon+
                      relev+
                      elev+
                      tchelsa1+tchelsa2+tchelsa3+tchelsa4+
                      w500+w50+w5000
                    , data = train, regression.points = geom(combpts)[,c("x", "y")], 
                    adaptive=F, bw=10,kernel = "gaussian", longlat=F)
  }else if(parm %in% c('tr')){
    gw <- gwr.basic(resid ~ lat+coslon+sinlon+
                      relev+
                      elev+
                      trchelsa1+trchelsa2+trchelsa3+trchelsa4+
                      w500+w50+w5000
                    , data = train, regression.points = geom(combpts)[,c("x", "y")], 
                    adaptive=F, bw=10,kernel = "gaussian", longlat=F)
  }else{
    gw <- gwr.basic(resid ~ lat+coslon+sinlon+
                      relev+
                      elev+
                      w500+w5000+windmean+
                      pchelsa1+pchelsa2+pchelsa3+pchelsa4+
                      (wind000)+(wind090)+(wind180)+(wind270)+
                      (wind045)+(wind135)+(wind225)+(wind315)
                    , data = train, 
                    regression.points = geom(combpts)[,c("x", "y")], 
                    adaptive=F, bw=10,kernel = "gaussian", longlat=F)

  }
  

  
  #extract gwr model covariates
  inames = intersect(names(gw$SDF),names(br10))
  combresults <- vect(gw$SDF)
  br.term0 <- combresults[0:nrow(newpts),]
  train.term0 <- combresults[-c(0:nrow(newpts)),] |> as.data.frame()
  train.cov <- as.data.frame(st_drop_geometry(train[,inames]))
  mod.2 <- train.cov*train.term0[,inames]
  mod.2 <- apply(mod.2, 1, sum) + train.term0[,'Intercept']
  train <- train |> mutate(resid2=z-(pred+mod.2))

  
  br.term <- rasterize(br.term0, y=br10, field=inames)
  Intercept <- rasterize(br.term0, y=br10, field='Intercept')
  # plot(br.term$wind180)
  br.term <- br.term |> project(br2)
  Intercept <- Intercept |> project(br2)
  br.cov <- br2[[inames]]
  plot(Intercept)
  gmodrast <- predict(br2, globm, na.rm=T, type = "response")
  plot(gmodrast)
  z0 <- sum(br.cov*br.term)+Intercept
  plot(z0)
  z <- z0 + gmodrast
  plot(z)
  # points(train, cex=0.1)
  
  # z1 <- gmodrast |> crop(ext(-161, -154, 18, 23))
  # plot(z1)
  # points(train, cex=0.1)
  # plot(br10$windmean*br10$w500)
  
  
  smoothdata <- train |> st_drop_geometry() |> 
    mutate(xgeo = paste(floor(x*10),'-',floor(y*10))) |> group_by(xgeo) |> summarise(x=mean(x), y=mean(y), elev=mean(elev), windmean=mean(windmean), z=mean(z),resid=mean(resid), resid2=mean(resid2)) |> ungroup() 
  
  xyz <-train[,c('x','y','resid2')] |> st_drop_geometry()
  gs <- gstat(formula=resid2~1, locations=~x+y, data=xyz, nmax=32, set=list(idp = 2))
  res1 <- interpolate(br10, gs, debug.level=0)[[1]] |> focalmed(r=50000, p='high')
  res1 <- res1 |> project(br2)
  plot(res1)
  z1 <- res1+z
  plot(z1)
  mmask <- ifel(br2$w50 >= 0.999, NA,1)
  z2 <- z1 * mmask
  plot(z2)
  names(z2) <- nameofpar
  writeRaster(z2, filnam, overwrite=T)
  
}




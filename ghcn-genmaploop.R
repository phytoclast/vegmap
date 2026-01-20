library(terra)
library(sf)
library(climatools)
library(dplyr)
library(fields)
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
# belts <- belts |> project(br1)
br1 <- c(br1,belts)
br1$tropical <- br1$monsoonE+br1$monsoonW+br1$tradeN+br1$tradeS
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
stations.p <- stations.p |> cbind(extract(belts,vect(stations.p)))
stations.t <- stations.t |> cbind(extract(belts,vect(stations.t)))
stations.tr <- stations.tr |> cbind(extract(belts,vect(stations.tr)))
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






gl <- c(-95, -75, 40, 50)
jp <- c(115, 145, 25, 45)
ak <- c(-170, -130, 55, 72)
chile <- c(-80, -55, -55, -30)
costarica <- c(-90, -70, 0, 15)
tas <- c(140, 155, -45, -35)
nz <- c(165, 180, -50, -30)
mex <- c(-115, -90, 15, 30)

lat.R <- seq(-90,90,7.5)
lon.R <- seq(-180,180,7.5)
lat.R <- lat.R[lat.R >= 0 & lat.R <= 90]
lon.R <- lon.R[lon.R >= -180 & lon.R <= -10]
addthis <- c(-15,15,-15,15)
parm = 'p'
mo = 1
final <- NULL
for(yi in 1:length(lat.R)){
  for(xi in 1:length(lon.R)){
    #yi = 5;xi = 5
    lat1 <- lat.R[yi]
    lon1 <- lon.R[xi]
    ex <-   c(lon1, lon1+15, lat1, lat1+15)
    
    exa <- ex + addthis
    
    if(parm %in% 't'){
      globl <- tdata
      globl$z <- globl |> select(tdatcols[mo]) |> st_drop_geometry() |> as.vector() |> unlist()
    }
    if(parm %in% 'tr'){
      globl <- trdata.
      globl$z <- globl |> select(trdatcols[mo]) |> st_drop_geometry() |> as.vector() |> unlist()
      # globl <- subset(globl, !(x > 10 & x < 30 & y > 45 & y < 60 & tr01 > 1.1))
    }
    if(parm %in% 'p'){
      globl <- pdata.
      globl$z <- globl |> select(pdatcols[mo]) |> st_drop_geometry() |> as.vector() |> unlist()
    }
    
    
    train <- subset(globl, x >= exa[1] & x <= exa[2] & y >= exa[3] & y <= exa[4]) 
    train <- train |> mutate(near = ifelse(x >= ex[1] & x <= ex[2] & y >= ex[3] & y <= ex[4],1,0)) |> mutate(wts = ifelse(near %in% 1, 100*wts0,1*wts0))
    mbr <- br1 |> crop(ext(ex[1],ex[2],ex[3],ex[4]))
    melev <- elev1 |> crop(mbr)
    thissample <- spatSample(mbr, 5000) |> subset(w50 <= 0.6, select=c("lat","coslon","sinlon","elev","w50","w500","w5000","relev","wind315","wind000","wind045","wind090","wind135","windmax","windmin","windmean"))
    if(nrow(thissample) > 100){
      if(max(thissample$elev) - min(thissample$elev)  > 100){
      #----plot(melev)

      myPCA <- prcomp(thissample, center = TRUE, scale. = TRUE)
      myPCA.r <- myPCA$rotation |> as.data.frame()
      myPCA.sd <- myPCA$sdev |> as.data.frame()
      myPCA.x <- myPCA$x |> as.data.frame()
      myPCA.x <- myPCA.x |> summarize(p1.8 = quantile(PC1,0.8),
                                      p1.6 = quantile(PC1,0.6),
                                      p1.4 = quantile(PC1,0.4),
                                      p1.2 = quantile(PC1,0.2),
                                      p2.8 = quantile(PC2,0.8),
                                      p2.6 = quantile(PC2,0.6),
                                      p2.4 = quantile(PC2,0.4),
                                      p2.2 = quantile(PC2,0.2),
                                      p3.8 = quantile(PC3,0.8),
                                      p3.6 = quantile(PC3,0.6),
                                      p3.4 = quantile(PC3,0.4),
                                      p3.2 = quantile(PC3,0.2))
      
      
      train <- train |> cbind(predict(myPCA, st_drop_geometry(train[,colnames(thissample)])))
      train <- train |> mutate(cat1 = case_when(PC1 >= myPCA.x$p1.8 ~'A',
                                                PC1 >= myPCA.x$p1.6 ~'B',
                                                PC1 >= myPCA.x$p1.4 ~'C',
                                                PC1 >= myPCA.x$p1.2 ~'D',
                                                TRUE ~'E'),
                               cat2 = case_when(PC2 >= myPCA.x$p2.8 ~'A',
                                                PC2 >= myPCA.x$p2.6 ~'B',
                                                PC2 >= myPCA.x$p2.4 ~'C',
                                                PC2 >= myPCA.x$p2.2 ~'D',
                                                TRUE ~'E'),
                               cat3 = case_when(PC3 >= myPCA.x$p3.8 ~'A',
                                                PC3 >= myPCA.x$p3.6 ~'B',
                                                PC3 >= myPCA.x$p3.4 ~'C',
                                                PC3 >= myPCA.x$p3.2 ~'D',
                                                TRUE ~'E')) |> 
        group_by(cat1) |> mutate(pcw1 = 100/length(cat1)) |>
        group_by(cat2) |> mutate(pcw2 = 100/length(cat2)) |> 
        group_by(cat3) |> mutate(pcw3 = 100/length(cat3)) |> ungroup() |> mutate(wts = pcw1+pcw2+pcw3,
                                                                                 wts = ifelse(near %in% 1, 
                                                                                              2*wts,1*wts))
    }}else{
      train <- train |> mutate(wts = wts0)
    }
    
    
    if(parm %in% c('t','tr')){
      globm = gam(z~s(coslon)+s(sinlon)+s(lat)+#
                    relev+
                    elev+
                    elev:w5000+
                    elev:tropical+
                    (w500)+(w5000)+(w50)
                  ,data=globl, weights = globl$wts)
    }else{
      globm = gam(z~s(coslon)+s(sinlon)+s(lat)+#
                    relev+
                    elev+
                    elev:w5000+
                    elev:tropical+
                    windmax+windmin+windmean+
                    (w500)+(w5000)+#
                    wind270:westerlyS+
                    wind270:westerlyN+
                    wind090:tradeN+
                    wind090:tradeS+
                    wind180:monsoonW+
                    wind180:monsoonE+
                    wind045:tradeN+
                    wind135:tradeS+
                    wind315:westerlyS+
                    wind225:westerlyN+
                    wind225:monsoonW+
                    wind135:monsoonE+
                    wind180:polarN+
                    wind000:polarS
                  ,data=globl, weights = globl$wts0)
    }
    summary(globm)
    train <- train |> mutate(pred = predict(globm, train, na.rm=T, type = "response"), resid = z-pred)
    if(parm %in% c('t','tr')){
      gm = gam(resid~s(coslon)+s(sinlon)+s(lat)+(elev)+(relev)+
                 (w500)+(w5000)+(w50)
               ,data=train, weights = train$wts)
    }else{
      gm = gam(resid~s(coslon)+s(sinlon)+s(lat)+(elev)+(relev)+
                 (w500)+(w5000)+#
                 windmax+windmin+windmean+
                 (wind000)+(wind090)+(wind180)+(wind270)+
                 (wind045)+(wind135)+(wind225)+(wind315)
               ,data=train, weights = train$wts)
      
    }
    summary(gm)
    
    summary(gm)
train <- train |> mutate(pred2 = predict(gm, train, na.rm=T, type = "response"), resid2 = z-(pred+pred2))
mod <- predict(mbr, globm, na.rm=T, type = "response")
# plot(mod)
mod2 <- predict(mbr, gm, na.rm=T, type = "response")
# plot(mod2)
mmask <- ifel(mbr$w50 >= 0.999, NA,1)
mod2 <- mod2 * mmask
     smoothdata <- subset(train, near %in% 1) |> mutate(xgeo = paste(floor(x*10),'-',floor(y*10))) |> group_by(xgeo) |> summarise(x=mean(x), y=mean(y), elev=mean(elev), windmean=mean(windmean), z=mean(z),resid=mean(resid), resid2=mean(resid2)) |> ungroup() 
    
    if(nrow(smoothdata) >= 10){
      
      xyz <-smoothdata[,c('x','y','elev')] |> st_drop_geometry()
      xy <-smoothdata[,c('x','y')] |> st_drop_geometry()
      v <- smoothdata$resid2#z
      tps <- Tps(xyz, v, lon.lat = TRUE)
      res1 <- interpolate(c(melev$elev), tps, xyOnly=F)#
      # tps <- Tps(xy, v, lon.lat = TRUE)
      # res1 <- interpolate(rast(melev), tps, xyOnly=T)#
      # plot(res1)
      res1 <- res1 |> project(mod)
      final0 <- res1+mod+mod2#
      # plot(final)
      #points(vect(train),cex=0.1)
      }else if(nrow(subset(train, near %in% 1)) > 0){
        smoothdata <- subset(train, near %in% 1) |> mutate(xgeo = paste(floor(x*10),'-',floor(y*10))) |> group_by(xgeo) |> summarise(x=mean(x), y=mean(y), elev=mean(elev), windmean=mean(windmean), z=mean(z),resid=mean(resid), resid2=mean(resid2)) |> ungroup()
        xyz <-smoothdata[,c('x','y','resid2')] |> st_drop_geometry()
        gs <- gstat(formula=resid2~1, locations=~x+y, data=xyz, nmax=5, set=list(idp = 0))
        res1 <- interpolate(melev, gs, debug.level=0)
        res1 <- res1 |> project(mod)
        final0 <- res1+mod+mod2
    }else{
      final0 <- mod+mod2
    }
    plot(final0)
    if(!is.null(final)){final <- climatools::spliceraster(final[[1]], final0[[1]])
    writeRaster(final, 'final_p01.tif', overwrite=T)
    }else{
      final <- final0
    }
    
  }}
plot(final0) 
points(smoothdata)
library(terra)
library(sf)
library(climatools)
library(dplyr)
library(fields)
library(gstat)
library(gam)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# pts <-  subset(globl, select=c(ID, NAME, STNELEV, x,y,elev, w50,w500,w5000,t01,t07))
# saveRDS(pts,'demopts.RDS')
pts0 <-  readRDS('demopts.RDS')

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
# th <- rast('terra2025/TerraClimate_19611990_tmax.nc')
# tl <- rast('terra2025/TerraClimate_19611990_tmin.nc')
# p <- rast('terra2025/TerraClimate_19611990_ppt.nc')
# 
# elev <- rast("C:/scripts/processclimategrids/wc2.1_2.5m_elev/wc2.1_2.5m_elev.tif")
# elev1 <- rast("C:/scripts/vegmap/chelsa2.1/dem1km.tif")
# elev <- rast("C:/scripts/vegmap/global/br5000.tif")[[1]]
# w50 <- rast("C:/scripts/vegmap/global/br5000.tif")[[2]]
altlayer <- rast("C:/scripts/vegmap/global/br5000.tif")[[c(1,2,8,9:16)]]
altlayer <- altlayer[[1:3]]
# ch<- rast("C:/scripts/vegmap/chelsa2.1/chelsa.tif")

cropto <- c(-95, -75, 40, 50)
cropto0 <- cropto + c(-5,5,-5,5)
grd <- crop(altlayer, cropto0)
xy0 <- climatools::makexyrast(grd[[1]],3)
grdall <- c(grd, xy0)
pts <- data.frame(x=pts0$x,y=pts0$y, z=pts0$t07)
pts <- pts |> subset(x >= cropto0[1] & x <= cropto0[2] &
                       y >= cropto0[3] & y <= cropto0[4])
vts <- vect(pts, geom=c("x", "y"),crs=crs('epsg:4326'))

vtsgrd <- terra::extract(grdall,vts)
pts <- cbind(pts,vtsgrd)
depvar <- names(pts)[3]
covars1 <- c(names(grd),names(xy0)[1:2])
f.glm <- stats::as.formula(paste(paste(depvar,paste(paste(covars1, collapse = " + ", sep = ""),""), sep = " ~ ")
))
segs <- 5 
spanx <- (cropto[2]-cropto[1])/segs
spany <- (cropto[4]-cropto[3])/segs
for(i.x in 1:segs){
  for(i.y in 1:segs){#i.x=1;i.y=3
    crop0 <- c(cropto[1]+(i.x-1)*spanx,cropto[1]+i.x*spanx,
               cropto[3]+(i.y-1)*spany,cropto[3]+i.y*spany)
    pts.i <- subset(pts, x >= crop0[1] & x <= crop0[2] &
                      y >= crop0[3] & y <= crop0[4])
    
    # if(nrow(pts.i) > 5 & max(pts.i[,'elev'])-min(pts.i[,'elev']) > 500){
    #   
    # }
    
    gm <- stats::glm(f.glm,
                 family='gaussian',
                 data=pts.i)
    summary(gm)
    cofs <- gm$coefficients
    pts <- 
  }
}

summary(gm)
plot(grd)
points(vts)























pts <- pts |> mutate(newelev = elev)
gm <- gam(t07 ~ x+s(y)+STNELEV+w50+w500+w5000, data=pts)
pts <- pts |> mutate(p1a = t07-predict.Gam(gm,pts))

gm <- gam(t07 ~ x+s(y)+newelev+w50+w500+w5000, data=pts)
pts <- pts |> mutate(p1b = t07-predict.Gam(gm,pts))

gm <- gam(t01 ~ x+s(y)+STNELEV+w50+w500+w5000, data=pts)
pts <- pts |> mutate(p2a = t01-predict.Gam(gm,pts))

gm <- gam(t01 ~ x+s(y)+newelev+w50+w500+w5000, data=pts)
pts <- pts |> mutate(p2b = t01-predict.Gam(gm,pts))

pts <- pts |> mutate(d1 = p1a^2+p2a^2,d2 = p1b^2+p2b^2, dd = d1-d2)
pts <- pts |> mutate(elev = ifelse(dd > 0, newelev, STNELEV))

gm <- gam(t01 ~ x+s(y)+STNELEV+w50+w500+w5000, data=pts)
1-gm$deviance/gm$null.deviance
gm <- gam(t01 ~ x+s(y)+newelev+w50+w500+w5000, data=pts)
1-gm$deviance/gm$null.deviance
gm <- gam(t01 ~ x+s(y)+elev+w50+w500+w5000, data=pts)
1-gm$deviance/gm$null.deviance

pts <- pts |> mutate(dd2 = elev-newelev)


ct <- c(-85, 45)
pwd <- c(0.5,1,2,3,5,7,10,15,20,25)
cti <- NULL
for(cti in 1:length(pwd)){
  
  wd <- c(1, 1)*pwd[cti]
  cov1 <- "elev"
  cov2 <- "w50"
  cov3 <- "w500"
  cov4 <- "w5000"
  xcoord <- "x"
  ycoord <- "y"
  zvar <- "t07"
  labl <- "NAME"
  dataset <- pts |> st_drop_geometry() |> as.data.frame()
  df <- data.frame(name=dataset[,labl],
                   x=dataset[,xcoord],
                   y=dataset[,ycoord],
                   v1=dataset[,cov1],
                   v2=dataset[,cov2],
                   v3=dataset[,cov3],
                   v4=dataset[,cov4],
                   z=dataset[,zvar])
  
  df1 <- df |> subset(x >= ct[1]-wd[1]  & x <= ct[1]+wd[1] & 
                        y >= ct[2]-wd[2]  & y <= ct[2]+wd[2] & 
                        !is.na(z) & !is.na(v1) & !is.na(v2))
  


  
  if(nrow(df1)>1){
    xsd <- sd(df1$x)
    ysd <- sd(df1$y)
    c1sd <- sd(df1$v1)
    c2sd <- sd(df1$v2)
    v1q9 <- quantile(df1$v1,0.9)
    v1q1 <- quantile(df1$v1,0.1)
    v1max <- max(df1$v1)
    v1min <- min(df1$v1)
    
    
    xysd <- (xsd*ysd)^0.5
    
    c1sdxy <- c1sd/xysd
    c2sdxy <- c2sd/xysd
    gm <- glm(z ~ x+y+v1+v2, data=df1)
    r2 = 1-gm$deviance/gm$null.deviance
    elevco <- gm$coefficients[4]
  }else{
    xsd <- 0
    ysd <- 0
    c1sd <- 0
    c2sd <- 0
    v1q9 <- 0
    v1q1 <- 0
    v1max <- max(df1$v1)
    v1min <- min(df1$v1)
    
    
    xysd <- (xsd*ysd)^0.5
    
    c1sdxy <- c1sd/xysd
    c2sdxy <- c2sd/xysd
    r2=0
    elevco=0
  }
  
  
  ndf0 <- data.frame(pwd=pwd[cti],
                     n = nrow(df1),
                     latmin = min(df1$y),
                     lat=ct[2],
                     latmax = max(df1$y),
                     lonmin =min(df1$x),
                     lon=ct[1],
                     lonmax = max(df1$x),                     
                     elevmin=v1min,
                     elevq1=v1q1,
                     elevq9=v1q9,
                     elevmax=v1max,                     
                     
                     elevsd=c1sd,
                     elevq=v1q9-v1q1,
                     elevmm=v1max-v1min,
                     watersd=c2sd,
                     
                     r2=r2,
                     elevco=elevco)
  if(cti==1){ ndf <- ndf0}else{ndf <- rbind(ndf,ndf0)}
}
ndf <- subset(ndf, n>1)
#cor(ndf[,c("c1sd","c1q","c1mm","c2sd","r2","elevco")],use='pairwise.complete.obs')
ggplot(df1, aes(x=z, y=v1))+
  geom_point()+
  geom_smooth()
summary(gm)



###################################3
df1 <- df |> subset(y > 0 & 
                      !is.na(z) & !is.na(v1) & !is.na(v2))
pars0 <- 0:10
pars1 <- data.frame(x=pars0)
pars2 <- data.frame(y=pars0)
pars3 <- data.frame(z=pars0)
pars <- pars1 |> merge(pars2) |> merge(pars3)
pars$r2 = 0
df1 <- df1 |> mutate(w = 0*v2+0*v3+0*v4)
for(i in 1:nrow(pars)){
  df1 <- df1 |> mutate(w = pars[i,1]*v2+pars[i,2]*v3+pars[i,3]*v4)
gm <- glm(z ~ y+v1+w, data=df1)
r2 <- 1-gm$deviance/gm$null.deviance
pars$r2[i] <- r2
}
  
df1 <- df1 |> mutate(w = 2*v2+5*v3+1*v4)

summary(gm)

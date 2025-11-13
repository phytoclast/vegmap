library(terra)
library(sf)
library(climatools)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
stations <- read.fwf('ghcn/ghcnd-stations.txt', widths = c(11,9,10,7,31))

tdata <- read.fwf('ghcn/v4/ghcnm.tavg.v4.0.1.20251106.qcf.dat',
                  widths = c(11,4,4,
                             5,1,1,1,
                             5,1,1,1,
                             5,1,1,1,
                             5,1,1,1,
                             5,1,1,1,
                             5,1,1,1,
                             5,1,1,1,
                             5,1,1,1,
                             5,1,1,1,
                             5,1,1,1,
                             5,1,1,1,
                             5,1,1,1), fill=T)
colnames(tdata) <- 
  c('ID','YEAR','ELEMENT',
    't01','DMFLAG1','QCFLAG1','DSFLAG1',
    't02','DMFLAG2','QCFLAG2','DSFLAG2',
    't03','DMFLAG3','QCFLAG3','DSFLAG3',
    't04','DMFLAG4','QCFLAG4','DSFLAG4',
    't05','DMFLAG5','QCFLAG5','DSFLAG5',
    't06','DMFLAG6','QCFLAG6','DSFLAG6',
    't07','DMFLAG7','QCFLAG7','DSFLAG7',
    't08','DMFLAG8','QCFLAG8','DSFLAG8',
    't09','DMFLAG9','QCFLAG9','DSFLAG9',
    't10','DMFLAG10','QCFLAG10','DSFLAG10',
    't11','DMFLAG11','QCFLAG11','DSFLAG11',
    't12','DMFLAG12','QCFLAG12','DSFLAG12')

tdata1 <- tdata[,c('ID','YEAR',
                  't01',
                  't02',
                  't03',
                  't04',
                  't05',
                  't06',
                  't07',
                  't08',
                  't09',
                  't10',
                  't11',
                  't12')] 
makena <- function(x){ifelse(x %in% -9999,NA,x)}


tdata1[,c(3:14)] <- sapply(tdata1[,c(3:14)], FUN=makena )
tdata1[,c(3:14)] <- tdata1[,c(3:14)]/100


stations1 <- read.fwf('ghcn/v3/ghcnm.tmax.v3.3.0.20190817.qca.inv', widths = c(11,9,10,7,31,5,1,5,2,2,2,2,1,2,16,1), comment.char = "")
stations2 <- read.fwf('ghcn/v3/ghcnm.tmin.v3.3.0.20190817.qca.inv', widths = c(11,9,10,7,31,5,1,5,2,2,2,2,1,2,16,1), comment.char = "")
colnames(stations1) <- c('ID','LATITUDE','LONGITUDE','STNELEV','NAME','GRELEV', 'POPCLS', 'POPSIZ', 'TOPO', 'STVEG', 'STLOC','OCNDIS','AIRSTN','TOWNDIS','GRVEG','POPCSS') 
colnames(stations2) <- c('ID','LATITUDE','LONGITUDE','STNELEV','NAME','GRELEV', 'POPCLS', 'POPSIZ', 'TOPO', 'STVEG', 'STLOC','OCNDIS','AIRSTN','TOWNDIS','GRVEG','POPCSS') 


thdata <- read.csv('ghcn/v3/ghcnm.tmax.v3.3.0.20170708.qca.dat.csv', header = F)
tldata <- read.csv('ghcn/v3/ghcnm.tmin.v3.3.0.20170708.qca.dat.csv', header = F)

colnames(thdata) <- 
  c('ID','YEAR','ELEMENT',
    'th01','DMFLAG1','QCFLAG1','DSFLAG1',
    'th02','DMFLAG2','QCFLAG2','DSFLAG2',
    'th03','DMFLAG3','QCFLAG3','DSFLAG3',
    'th04','DMFLAG4','QCFLAG4','DSFLAG4',
    'th05','DMFLAG5','QCFLAG5','DSFLAG5',
    'th06','DMFLAG6','QCFLAG6','DSFLAG6',
    'th07','DMFLAG7','QCFLAG7','DSFLAG7',
    'th08','DMFLAG8','QCFLAG8','DSFLAG8',
    'th09','DMFLAG9','QCFLAG9','DSFLAG9',
    'th10','DMFLAG10','QCFLAG10','DSFLAG10',
    'th11','DMFLAG11','QCFLAG11','DSFLAG11',
    'th12','DMFLAG12','QCFLAG12','DSFLAG12')

colnames(tldata) <- 
  c('ID','YEAR','ELEMENT',
    'tl01','DMFLAG1','QCFLAG1','DSFLAG1',
    'tl02','DMFLAG2','QCFLAG2','DSFLAG2',
    'tl03','DMFLAG3','QCFLAG3','DSFLAG3',
    'tl04','DMFLAG4','QCFLAG4','DSFLAG4',
    'tl05','DMFLAG5','QCFLAG5','DSFLAG5',
    'tl06','DMFLAG6','QCFLAG6','DSFLAG6',
    'tl07','DMFLAG7','QCFLAG7','DSFLAG7',
    'tl08','DMFLAG8','QCFLAG8','DSFLAG8',
    'tl09','DMFLAG9','QCFLAG9','DSFLAG9',
    'tl10','DMFLAG10','QCFLAG10','DSFLAG10',
    'tl11','DMFLAG11','QCFLAG11','DSFLAG11',
    'tl12','DMFLAG12','QCFLAG12','DSFLAG12')

thdata <- thdata[,c('ID','YEAR',
  'th01',
  'th02',
  'th03',
  'th04',
  'th05',
  'th06',
  'th07',
  'th08',
  'th09',
  'th10',
  'th11',
  'th12')]
tldata <- tldata[c('ID','YEAR',
  'tl01',
  'tl02',
  'tl03',
  'tl04',
  'tl05',
  'tl06',
  'tl07',
  'tl08',
  'tl09',
  'tl10',
  'tl11',
  'tl12')]
trdata <- thdata |> left_join(tldata)

makena <- function(x){ifelse(x %in% -9999,NA,x)}

trdata[,c(3:26)] <- sapply(trdata[,c(3:26)], FUN=makena )
trdata[,c(3:26)] <- trdata[,c(3:26)]/100


trdata <- trdata |> mutate(tr01=abs(th01-tl01),
                           tr02=abs(th02-tl02),
                           tr03=abs(th03-tl03),
                           tr04=abs(th04-tl04),
                           tr05=abs(th05-tl05),
                           tr06=abs(th06-tl06),
                           tr07=abs(th07-tl07),
                           tr08=abs(th08-tl08),
                           tr09=abs(th09-tl09),
                           tr10=abs(th10-tl10),
                           tr11=abs(th11-tl11),
                           tr12=abs(th12-tl12))
nstations <- read.fwf('1981-2010/ghcnd-stations.txt', widths = c(11,1,8,1,9,1,6,1,2,1,30,1,3,1,3,1,5,1,13), comment.char = "")
colnames(nstations) <- c('ID','x1','LATITUDE','x2','LONGITUDE','x3','STNELEV','x4','STATE','x5','NAME','x6','GSNFLAG','x7','HCNFLAG','x8','WMOID','x9','METHOD')
keep <- c('ID','LATITUDE','LONGITUDE','STNELEV','NAME','STATE')
nstations <- nstations[,keep]
nthdata <- read.fwf('1981-2010/mly-tmax-filled.txt', widths = c(11,1,4,1,
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
ntldata <- read.fwf('1981-2010/mly-tmin-filled.txt', widths = c(11,1,4,1,
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
colnames(ntldata) <- 
  c('ID','FLAG','YEAR','FLAG',
    'tl01','FLAG',
    'tl02','FLAG',
    'tl03','FLAG',
    'tl04','FLAG',
    'tl05','FLAG',
    'tl06','FLAG',
    'tl07','FLAG',
    'tl08','FLAG',
    'tl09','FLAG',
    'tl10','FLAG',
    'tl11','FLAG',
    'tl12','FLAG')
colnames(nthdata) <- 
  c('ID','FLAG','YEAR','FLAG',
    'th01','FLAG',
    'th02','FLAG',
    'th03','FLAG',
    'th04','FLAG',
    'th05','FLAG',
    'th06','FLAG',
    'th07','FLAG',
    'th08','FLAG',
    'th09','FLAG',
    'th10','FLAG',
    'th11','FLAG',
    'th12','FLAG')
nthdata <- nthdata[,c('ID','YEAR',
                    'th01',
                    'th02',
                    'th03',
                    'th04',
                    'th05',
                    'th06',
                    'th07',
                    'th08',
                    'th09',
                    'th10',
                    'th11',
                    'th12')]
ntldata <- ntldata[,c('ID','YEAR',
                    'tl01',
                    'tl02',
                    'tl03',
                    'tl04',
                    'tl05',
                    'tl06',
                    'tl07',
                    'tl08',
                    'tl09',
                    'tl10',
                    'tl11',
                    'tl12')]
makena <- function(x){ifelse(x %in% -9999,NA,x)}
nthdata[,c(3:14)] <- sapply(nthdata[,c(3:14)], FUN=makena )
nthdata[,c(3:14)] <- nthdata[,c(3:14)]/100
ntldata[,c(3:14)] <- sapply(ntldata[,c(3:14)], FUN=makena )
ntldata[,c(3:14)] <- ntldata[,c(3:14)]/100

ntdata <- nthdata |> left_join(ntldata)

ntdata <- ntdata |> mutate(t01=(th01+tl01)/2,
                           t02=(th02+tl02)/2,
                           t03=(th03+tl03)/2,
                           t04=(th04+tl04)/2,
                           t05=(th05+tl05)/2,
                           t06=(th06+tl06)/2,
                           t07=(th07+tl07)/2,
                           t08=(th08+tl08)/2,
                           t09=(th09+tl09)/2,
                           t10=(th10+tl10)/2,
                           t11=(th11+tl11)/2,
                           t12=(th12+tl12)/2,
                           tr01=abs(th01-tl01),
                           tr02=abs(th02-tl02),
                           tr03=abs(th03-tl03),
                           tr04=abs(th04-tl04),
                           tr05=abs(th05-tl05),
                           tr06=abs(th06-tl06),
                           tr07=abs(th07-tl07),
                           tr08=abs(th08-tl08),
                           tr09=abs(th09-tl09),
                           tr10=abs(th10-tl10),
                           tr11=abs(th11-tl11),
                           tr12=abs(th12-tl12))

tdata2 <- tdata1 |> rbind(ntdata[,colnames(tdata1)])
saveRDS(tdata2, 'ghcn/tdata.RDS')


trdata2 <- trdata |> rbind(ntdata[,colnames(trdata)])
saveRDS(trdata2, 'ghcn/trdata.RDS')
comcol <- intersect(colnames(nstations),colnames(stations1))
nstations<-nstations |> mutate(NAME = paste(STATE, NAME))
trstations <- stations1[,comcol] |> rbind(nstations[,comcol])
comcol <- intersect(colnames(nstations),colnames(stations))
tstations <- stations[,comcol] |> rbind(nstations[,comcol])
write.csv(tstations, 'ghcn/stations.t.csv', row.names = FALSE)
write.csv(trstations, 'ghcn/stations.tr.csv', row.names = FALSE)

###############

library(terra)
library(sf)
library(climatools)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
elev <- rast('global/elev.tif')
wind <- rast('global/wind.tif')
br <- rast('ghcn/br.tif')
stations.t <- read.csv('ghcn/stations.t.csv') |> unique()
stations.t <- stations.t |> group_by(ID, LATITUDE, LONGITUDE, STNELEV) |> summarize(NAME=first(NAME)) 


stations.t <- stations.t |> mutate(y=LATITUDE,x=LONGITUDE) |> st_as_sf(coords = c(x='LONGITUDE',y='LATITUDE'), crs=st_crs(elev))
tdata <- readRDS('ghcn/tdata.RDS') |> unique()
thesecols <- colnames(tdata)[3:14]
tdata <- tdata |> group_by(ID, YEAR) |> 
  summarize(across(all_of(thesecols), \(x) mean(x, na.rm = TRUE)))
tdata <- tdata |> ungroup()
stations.t <- ungroup(stations.t)
replaceNANs <- function(x){
  x=ifelse(is.nan(x), NA,x)
  return(x)
}
tdata[,3:14] <- sapply(tdata[,3:14],'replaceNANs')

newelev <- extract(elev, vect(stations.t))
newwind <- extract(wind, project(vect(stations.t), wind))
newbr <- extract(br, project(vect(stations.t), br))

stations.t <- stations.t |> mutate(elev = newelev$elev)
stations.t <- stations.t |> 
  mutate(STNELEV = case_when(STNELEV %in% c(-999,-999.9) ~ elev,
                                       STNELEV == 0 ~ elev, 
                                       TRUE ~ STNELEV))

stations.t <- stations.t |> mutate(elev = STNELEV,
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
###############tr
stations.tr <- read.csv('ghcn/stations.tr.csv')
stations.tr <- stations.tr |> mutate(y=LATITUDE,x=LONGITUDE) |> st_as_sf(coords = c(x='LONGITUDE',y='LATITUDE'), crs=st_crs(elev))
trdata <- readRDS('ghcn/trdata.RDS')
newelev <- extract(elev, vect(stations.tr))
newwind <- extract(wind, project(vect(stations.tr), wind))
newbr <- extract(br, project(vect(stations.tr), br))
stations.tr <- stations.tr |> mutate(elev = newelev$elev)
stations.tr <- stations.tr |> 
  mutate(STNELEV = case_when(STNELEV %in% c(-999,-999.9) ~ elev,
                             STNELEV == 0 ~ elev, 
                             TRUE ~ STNELEV))
stations.tr <- stations.tr |> mutate(elev = STNELEV,
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
saveRDS(tdata, 'ghcn/tdata2.RDS')
saveRDS(trdata, 'ghcn/trdata2.RDS')
saveRDS(stations.t, 'ghcn/stations.t2.RDS')
saveRDS(stations.tr, 'ghcn/stations.tr2.RDS')


######start models
library(terra)
library(sf)
library(climatools)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

tdata <- readRDS('ghcn/tdata2.RDS') 
trdata <- readRDS('ghcn/trdata2.RDS') 
stations.t <- readRDS('ghcn/stations.t2.RDS')
stations.tr <- readRDS('ghcn/stations.tr2.RDS')
tdata <- stations.t |> left_join(tdata)
trdata <- stations.tr |> left_join(trdata)

library(gam)
br2 <- rast('ghcn/br.tif')
elev2 <- rast('global/elev.tif') |> aggregate(fact=20)
br2$elev <- elev2
wind2 <-rast('ghcn/wind.tif')
br2 <- c(br2, wind2)
br2$YEAR <- 1990
tdata <- tdata |> mutate(elevzone = floor(((elev/5000)+0.1)^0.5*5), 
                         lzone = floor((x/180+1)*50)*10+floor((y/90+1)*5)*100) |> group_by(elevzone, lzone) |> mutate(wts = 100/(length(NAME)+1)*100/((YEAR-1976)^2+15^2)) |> ungroup()
trdata <- trdata |> mutate(elevzone = floor(((elev/5000)+0.1)^0.5*5), 
                         lzone = floor((x/180+1)*50)*10+floor((y/90+1)*5)*100) |> group_by(elevzone, lzone) |> mutate(wts = 100/(length(NAME)+1)*100/((YEAR-1976)^2+15^2)) |> ungroup()


# lat <- br2$lat
# x <- (-1800:1800)/10; y <- (-900:900)/10
# xy <- merge(x, y)
# lon <- rast(data.frame(x=xy$x,y=xy$y, z=xy$x), type="xyz", crs=crs(br2))
# lon <- project(lon, br2)
# sinlon = sin(br2$/360*2*pi), 
# coslon = cos(x/360*2*pi),
df <- data.frame(ID=c('MI','Yucatan','CA','AK','Amazon','Chile')
                 ,x=c(-84, -90, -115, -135,-70,-70),
                 y=c(45, 15, 35, 50,-15,-45))
df <- vect(df,geom=c(x='x',y='y'), crs=crs(br2))
for(i in 1:nrow(df)){
  rname = paste0('r',i)
  dfr <- df[i,] |>rasterize(br2, field=1)
  d1 <- terra::distance(dfr, unit="km") 
  d2 <- ifel(d1 < 1000,1,0) |> focalmed(r=5)
  names(d2) <- rname
  assign(rname, d2)
}
br4 <-  c(br2, r1,r2,r3,r4,r5)
rvals <- c(r1,r2,r3,r4,r5)
getrvals <- extract(rvals, vect(stations.t))
rvaldf <- stations.t |> cbind(getrvals[,2:6]) |> st_drop_geometry() 
tdata1 <- tdata |> left_join(unique(rvaldf[,c('ID','r1','r2','r3','r4','r5')]))
newext <- ext(-170,-60, 30, 75)
br3 <- br4 |> crop(newext)
# br3$elev<-3000
train0 <- tdata1 |> mutate(z = t01) |> subset(!is.na(z)) 
train2 <- train0 |> subset(x >= newext[1] & x <= newext[2] & y >= newext[3] & y <= newext[4])
train1 <- train0[sample(1:nrow(train0), size=500, prob=train0$wts, replace = TRUE),]
train2 <- train2[sample(1:nrow(train2), size=20000, prob=train2$wts, replace = TRUE),]
train <- rbind(train2)
trainC <- train |> mutate(z=z)#log2(z+1))
#trainC <- train |> mutate(z=log2(z+1))
gmC <- gam(
  z ~ s(lat) + s(sinlon) + s(coslon)+
    s(elev)+(w50)+(w500) +(w5000) + 
    s(wind000)+ 
    s(wind045)+  
    s(wind090)+  
    s(wind135)+  
    s(wind180)+  
    s(wind225)+  
    s(wind270)+  
    s(wind315)+
    s(YEAR)+
    
    r1:s(lat) + r1:s(sinlon) + r1:s(coslon)+
    r1:s(elev)+r1:(w50)+r1:(w500) +r1:(w5000) + 
    r1:s(wind000)+ 
    r1:s(wind045)+  
    r1:s(wind090)+  
    r1:s(wind135)+  
    r1:s(wind180)+  
    r1:s(wind225)+  
    r1:s(wind270)+  
    r1:s(wind315)+
    r1:s(YEAR)+
    
    r2:s(lat) + r2:s(sinlon) + r2:s(coslon)+
    r2:s(elev)+r2:(w50)+r2:(w500) +r2:(w5000) + 
    r2:s(wind000)+ 
    r2:s(wind045)+  
    r2:s(wind090)+  
    r2:s(wind135)+  
    r2:s(wind180)+  
    r2:s(wind225)+  
    r2:s(wind270)+  
    r2:s(wind315)+
    r2:s(YEAR)+
    
    r3:s(lat) + r3:s(sinlon) + r3:s(coslon)+
    r3:s(elev)+r3:(w50)+r3:(w500) +r3:(w5000) + 
    r3:s(wind000)+ 
    r3:s(wind045)+  
    r3:s(wind090)+  
    r3:s(wind135)+  
    r3:s(wind180)+  
    r3:s(wind225)+  
    r3:s(wind270)+  
    r3:s(wind315)+
    r3:s(YEAR)+
    
    r4:s(lat) + r4:s(sinlon) + r4:s(coslon)+
    r4:s(elev)+r4:(w50)+r4:(w500) +r4:(w5000) + 
    r4:s(wind000)+ 
    r4:s(wind045)+  
    r4:s(wind090)+  
    r4:s(wind135)+  
    r4:s(wind180)+  
    r4:s(wind225)+  
    r4:s(wind270)+  
    r4:s(wind315)+
    r4:s(YEAR)+
    
    r5:s(lat) + r5:s(sinlon) + r5:s(coslon)+
    r5:s(elev)+r5:(w50)+r5:(w500) +r5:(w5000) + 
    r5:s(wind000)+ 
    r5:s(wind045)+  
    r5:s(wind090)+  
    r5:s(wind135)+  
    r5:s(wind180)+  
    r5:s(wind225)+  
    r5:s(wind270)+  
    r5:s(wind315)+
    r5:s(YEAR)
  , data=trainC, family = 'gaussian')

summary(gmC)
br3$elev<-4000
prC <- predict(br3, gmC, na.rm=T, type = "response")
# prC <- 2^prC-1
plot((prC))
points(st_geometry(train), cex=0.5)
e4000<-prC
e2000<-prC
e0000<-prC
plot(e4000-e2000)
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
newgrid <- 2^newgrid-1
plot(newgrid)


########
antarctic <- subset(tdata, elev >= 2000) |>group_by(ID,elev,NAME,y,x) |> summarise(t01=mean(t01,na.rm=T),t07=mean(t07,na.rm=T))
br4 <- br2
br4$elev <- 3000
plot(br2$elev)
points(st_geometry(antarctic), cex=0.5)


library(sf)
library(terra)
library(ranger)
library(rpart)
library(rpart.plot)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path = 'C:/workspace2/processclimategrids/output/'
MAAT <- rast(paste0(path, 'MAAT.tif')); names(MAAT)<-'MAAT'
bt <- rast(paste0(path, 'bt.tif')); names(bt)<-'bt'
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
# md <- (e-d)/(e+0.0001); names(md)<-'md'; #writeRaster(md,paste0(path, 'md.tif'))
mholdridge <- p/(bt*58.93+0.001); names(mholdridge) <- 'mholdridge'

chelsa2 <-rast('chelsa2.1/chelsa2.tif')
e <- chelsa2$e |> project(Tw)
p <- chelsa2$p |> project(Tw)
d <- chelsa2$d |> project(Tw)
s <- chelsa2$s |> project(Tw)
max3aet <- chelsa2$max3aet |> project(Tw)
md <- (1-d); names(md)<-'md'
m <- (p/(p+e+0.001)); names(m)<-'m'

plot(m)


rastbrick <- c(Tw,Twh,Tg,Tc,Tclx,m, md, bt, mholdridge, s,d,e,p,MAAT, max3aet,slope, hydric, elev, sealevel,clay, sand,marine,soilpH,bedrock)



# pcabrick <- rastbrick[[c('Tw', 'Twh', 'Tg', 'Tc', 'Tclx', 'm', 'p3AET', 'md', 
#                          'slope', 'hydric', 'sealevel', 'clay', 'sand', 'soilpH', 'bedrock')]]
# pcabrick <- terra::princomp(pcabrick)

eco <- st_read('data/ecoregions.shp')
ecoalt <- read.csv('data/wwfeco.types.cover2.csv')
biome2023 <- read.csv('data/biome2023.csv',  na.strings = FALSE, fileEncoding = 'latin1') |> subset(select=c(ECO_ID, altbiom2023, tclim, vstr)) |> unique()
eco <- eco %>% subset(!BIOME %in% c(98))
ecoids <- eco$ECO_ID %>% unique()


#----




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
ecopts.repro <- ecopts.repro %>% st_as_sf() |> subset(select=c(ECO_ID)) |>
  left_join(unique(subset(biome2023, select=c('ECO_ID','altbiom2023','tclim','vstr'))), relationship = "many-to-many") 



ecopts1 <- rastbrick %>% extract(ecopts.repro)
ecopts2 <- ecopts.repro #%>% st_drop_geometry()
ecopts4 <- cbind(ecopts2, ecopts1) 
ecopts4 <- subset(ecopts4, select=c(ECO_ID,altbiom2023,tclim, vstr, Tw,Twh,Tg,Tc,Tclx,m, md, MAAT, bt, mholdridge, s,d,e,p, max3aet,slope,hydric,sealevel,clay,sand,marine,soilpH,bedrock)) 

ecopts5 <- ecopts4
ecopts5 <-  subset(ecopts5, !is.na(altbiom2023) & !is.na(Tw) & !is.na(m) & !is.na(Tclx) & !is.na(Tg) &!is.na(max3aet) &!is.na(slope) &!is.na(hydric) &!is.na(sand))


ecopts5 <-  subset(ecopts5, !(Tg > 9 & vstr %in% c("T"))
                   & !(Tg < 3 & !vstr %in% c("T"))
                   & !(m > 0.5 & vstr %in% c("D"))
                   & !(Tclx < -2 & tclim %in% c(1))
                   & !(Tclx > 0 & tclim %in% c(6,7,8)))
ecopts5 <-  mutate(ecopts5, vstr=case_when(vstr %in% "F" & m < 0.4 ~ "Fd",
                                           vstr %in% "F" & m > 0.5 & d > 0.6 & max3aet > 180 ~ "Fm",
                                           vstr %in% "F" & m > 0.5 & d > 0.6 & max3aet < 180 ~ "Fp",
                                           vstr %in% c("Fp","Fd","Fm") & m > 0.6 & d < 0.3 ~ "F",
                                           TRUE ~ vstr))



writeRaster(rastbrick, 'rastbrick.tif', overwrite =T)




BPS <- rast('data/BPS.tif') 
bpspts <- spatSample(BPS, 20000, xy=TRUE)
bpsalt <- read.csv('data/bps.types.cover2.csv')
bpsclimate <- read.csv('data/bpsclimate.csv')
bpsrat <- foreign::read.dbf('data/BPS.dbf')
bpsrat <- bpsrat %>% left_join(bpsalt)
bpspts <- bpspts %>% left_join(bpsrat, by=join_by(BPS == VALUE))
bpspts <- bpspts |> subset(!is.na(BPS_MODEL) & !BPS_MODEL %in% 'na')
bpspts <- bpspts |> st_as_sf(coords = c(x='x',y='y'), crs=crs(BPS))
bpspts <- bpspts |> st_transform(crs=crs(rastbrick))
bpsenv <- rastbrick |> extract(bpspts)
bpspts <- bpspts |> cbind(bpsenv)
bpspts <- bpspts |> left_join(subset(bpsclimate, select=c(BPS_NAME, tclim, vstr)))

# bpssum <- bpspts |> group_by(BPS_NAME) |> summarise(Tg = median(Tg),Tc = median(Tc), m = median(m), md = median(md), max3aet = median(max3aet))

# ecosum <- ecopts5 |> group_by(REALM,BIOME,ECO_ID,ECO_NAME,altbiom2023) |> summarise(Tg = median(Tg),Tc = median(Tc), Tclx = median(Tclx), m = median(m), md = median(md), max3aet = median(max3aet)) |> st_drop_geometry()
# 
# write.csv(ecosum,'data/ecosum.csv',row.names = F)
commoncols <- intersect(colnames(bpspts),colnames(ecopts5))
ecopts6 <- subset(ecopts5, select=commoncols) |> rbind(subset(bpspts, select=commoncols))

saveRDS(ecopts6, 'ecopts5.RDS')


#### ----
#start
library(sf)
library(terra)
library(ranger)
library(rpart)
library(rpart.plot)
library(dplyr)
# library(mgcv)
library(gam)
library(maxnet)
library(stringr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

ecopts5 <- readRDS('ecopts5.RDS')
ecopts5 <-  mutate(ecopts5, tclim=case_when(tclim %in% 4 & Tg < 16 ~ 5,
                                            tclim %in% 6 & Tg < 10 ~ 7,
                                            tclim %in% c(1,2) & Tg < 5 ~ 3,
                                            tclim %in% c(4,5,6,7) & Tg < 5 ~ 8,
                                           TRUE ~ tclim))

rastbrick <- rast('rastbrick.tif')
ecopts6 <- ecopts5 |> st_drop_geometry()
smoothvars <- c('Tw', 'Twh', 'Tg', 'Tc', 'Tclx', 'm', 'max3aet', 'md', 
                'slope', 'hydric', 'sealevel', 'clay', 'sand', 'soilpH', 'bedrock')

formular.gam <- as.formula(paste(paste("pos",paste(paste("s(",smoothvars,")", collapse = " + ", sep = ""),""), sep = " ~ ")
))
formular.glm <- as.formula(paste(paste("pos",paste(paste("I(",smoothvars,"^2)", collapse = " + ", sep = ""),""), sep = " ~ ")
))
scope.gam <- paste("~",paste("s(",smoothvars,")", collapse = " + ", sep = ""))
formular.scope.gam <- as.formula(paste("pos",paste(smoothvars, collapse = " + ", sep = ""), sep = " ~ "))

formular.rf <- as.formula(paste(paste("pos",paste(paste(smoothvars, collapse = " + ", sep = ""),""), sep = " ~ ")
))

ecopts6 <- ecopts6 |>  mutate(vstr=as.factor(vstr)) |> subset(!is.na(slope) & !is.na(Tw) & !is.na(vstr) & !is.na(tclim))
vstrs <- unique(ecopts6$vstr)

for (i in 1:length(vstrs)){#i=1
  ecopos <- ecopts6 |> subset(vstr %in% vstrs[i]) |> mutate(pos=as.numeric(vstrs[i]))
  if(vstrs[i] %in% c('Fh','Gh')){
    ecopos <- ecopos[sample(1:nrow(ecopos), size=2200, replace=T),] 
    takeout.p <- sample(1:nrow(ecopos), size=200, replace=F)
    }else{
  ecopos <- ecopos[sample(1:nrow(ecopos), size=2200, replace=T),] 
  takeout.p <- sample(1:nrow(ecopos), size=200, replace=F)}

  trainpos <- ecopos[-takeout.p,]
  testpos <- ecopos[takeout.p,] 
  if(i==1){
    train <- trainpos
    test <- testpos
  }else{
    train <- rbind(train,trainpos)
    test <- rbind(test,testpos)
  }
}
key <- data.frame(train$vstr, train$pos) |> unique()



rf <- ranger(formular.rf,
             data=train, classification = TRUE)
test.rf <- test |> mutate(prediction = predictions(predict(rf, test, na.rm=T)))
Metrics::accuracy(actual=test.rf$pos, predicted=test.rf$prediction)

rf.prediction <-  predict(rastbrick, rf, na.rm=T);  names(rf.prediction) <- 'vstr'
writeRaster(rf.prediction, paste0('biomes3/vstr.tif'), overwrite=T)
plot(rf.prediction)

test0 <- test |> mutate(open = ifelse(vstr %in% c('S'), 1,0), forest=ifelse(vstr %in% c('G'), 1,0)) |> subset(forest+open > 0)
test.thresh <- climatools::find.multithreshold(test0,'forest','open',c(smoothvars))


test0 <- test |> mutate(tropical = ifelse(vstr %in% c('F','Fd','Fp','Fm'), 1,0), temperate=ifelse(vstr %in% c('G','W'), 1,0), temperate=1-tropical, Trange = Tg-Tc, Tc12 = pmin(Tclx+12,Tc), Tc15 = pmin(Tclx+15,Tc), txrange = Tg - Tc15, Tc15r = pmax(pmin(Tclx+15,Tc), 20-(Trange)),Tc15rx = pmax(pmin(Tclx+15,Tc), 20-(txrange))) |> subset(tropical+temperate > 0 & m >0.4 & m < 0.6 & Tg > 12)
test.thresh <- climatools::find.multithreshold(test0,'tropical','temperate',c(smoothvars,'Trange','Tc12','Tc15','Tc15r','txrange','Tc15rx'))

test0 <- ecopts5 |> mutate(lat = st_coordinates(ecopts5)[,2], tropical = ifelse(lat <= 23.5 & lat >= -23.5, 1,0), temperate=1-tropical, Trange = Tg-Tc, Tc12 = pmin(Tclx+12,Tc), Tc15 = pmin(Tclx+15,Tc), txrange = Tg - Tc15, Tc15r = pmax(pmin(Tclx+15,Tc), 20-(Trange)),Tc15rx = pmax(pmin(Tclx+15,Tc), 20-(txrange))) |> subset(m > 0.5)
test.thresh <- climatools::find.multithreshold(test0,'tropical','temperate',c(smoothvars,'Trange','Tc12','Tc15','Tc15r','txrange','Tc15rx'))
Tc15 <- min(rastbrick$Tc, rastbrick$Tclx+15)
trange <- rastbrick$Tg - Tc15

talt <- (rastbrick$Tg > 6)+(rastbrick$Tg > 12)+(rastbrick$Tg > 18)+(rastbrick$Tg > 24)
tclim <- ifel(Tc15 >= 15 | trange < 5,10,ifel(Tc15 >= 0 | (trange < 15 & rastbrick$Tg < 15 & Tc15 >= -3), 20,30))+talt
plot(tclim)
writeRaster(tclim,'tclim.tif',overwrite=T)
library(ggplot2)

trainx <-  trainx |> mutate(vstrs =  ifelse(vstr %in% c('F','Fd','Fp','Fm'), 'F', as.character(vstr))) |> subset(!vstr %in% c('Fh','Gh','T'))

ggplot(data=trainx, aes(x=m, y=vstrs))+
  geom_area()
  

formular.rf <- as.formula(paste(paste("pos",paste(paste(smoothvars, collapse = " + ", sep = ""),""), sep = " ~ ")
))
trainx <- train |> st_drop_geometry()
testx <- train |> st_drop_geometry()
classes <- unique(train$vstr)
for(i in 1:length(classes)){#i=1
  trainx <- trainx |> mutate(pos=ifelse(vstr %in% classes[i],1,0))
rf <- ranger(formular.rf,
             data=trainx, classification = F)

testx <-  testx |> mutate(x = predictions(predict(rf, trainx, na.rm=T)))
colnames(testx)[colnames(testx)%in% 'x'] = as.character(classes[i])
}

testx <- testx |> mutate(slopeclass = ifelse(slope > 1.5,1,0), mclass = floor(m*10))
testxsum <- testx |> subset(Tg >= 6 & hydric < 25) |> group_by(slopeclass, mclass) |> summarize(F = mean(F+Fd+Fp+Fm),W = mean(W),G = mean(G),S = mean(S),D = mean(D)) |> mutate(total = F+W+G+S+D) |> 
  mutate(F= round(F/total*100, 1),W= round(W/total*100, 1),G= round(G/total*100, 1),S= round(S/total*100, 1),D= round(D/total*100, 1))


saveRDS(testx, 'testx.RDS')








summary(gl)


gl.prediction <-  predict(rastbrick, gl, na.rm=T, type = "response");  names(gl.prediction) <- 'tropics'

plot(gl.prediction)
writeRaster(gl.prediction, 'tropics.tif', overwrite=T)

tclims <- unique(ecopts6$tclim)
tclims; as.numeric(tclims)
for (i in 1:length(tclims)){#i=1
  ecopos <- ecopts6 |> subset(tclim %in% tclims[i]) |> mutate(pos=as.numeric(tclims[i]))
  
  ecopos <- ecopos[sample(1:nrow(ecopos), size=2200, replace=T),] 
  takeout.p <- sample(1:nrow(ecopos), size=200, replace=F)
  trainpos <- ecopos[-takeout.p,]
  testpos <- ecopos[takeout.p,] 
  if(i==1){
    train <- trainpos
    test <- testpos
  }else{
    train <- rbind(train,trainpos)
    test <- rbind(test,testpos)
  }
}
key2 <- data.frame(train$tclim, train$pos) |> unique()



rf <- ranger(formular.rf,
             data=train, classification = TRUE)
test.rf <- test |> mutate(prediction = predictions(predict(rf, test, na.rm=T)))
Metrics::accuracy(actual=test.rf$pos, predicted=test.rf$prediction)

rf.prediction <-  predict(rastbrick, rf, na.rm=T);  names(rf.prediction) <- 'tclim'
writeRaster(rf.prediction, paste0('biomes3/tclim.tif'), overwrite=T)
plot(rf.prediction)


# #determine optimal formulas ----
# intercept_only <- gam(pos ~ 1, data=train)
# 
# scopelist <- gam.scope(train[,c('pos',smoothvars)], response = 1, smoother = "s", arg = c("df=2","df=4"), form = TRUE)
# #define model with all predictors
# 
# #perform forward stepwise regression
# forward <- step.Gam(intercept_only, direction='both', scope=scopelist, trace=1)
# forward$formula
# 
# gm <- gam(forward$formula,
#              family='binomial',
#              data=train)
# summary(gm)
# 
# test.gam <- test |> mutate(prediction = predict(gm, test, na.rm=T, type = "response"))
# Metrics::rmse(actual=test.gam$pos, predicted=test.gam$prediction)
# 
# 
# intercept_only <- glm(pos ~ 1, data=train)
# 
# formular.glm <- as.formula(paste(paste("pos",paste(paste(smoothvars,"+I(",smoothvars,"^2)","+I(",smoothvars,"^4)", collapse = " + ", sep = ""),""), sep = " ~ ")
# ))
# 
# forward <- step(intercept_only, direction='both', scope=formular.glm, trace=1)
# forward$formula
# 
# gl <- glm(forward$formula,
#           family='binomial',
#           data=train)
# summary(gl)
# test.glm <- test |> mutate(prediction = predict(gl, test, na.rm=T, type = "response"))
# Metrics::rmse(actual=test.glm$pos, predicted=test.glm$prediction)



# gm <- gam(formular.gam,
#           family='binomial',
#           data=train)
# summary(gm)
# test.gam <- test |> mutate(prediction = predict(gm, test, na.rm=T, type = "response"))
# Metrics::rmse(actual=test.gam$pos, predicted=test.gam$prediction)
# gm.prediction <-  predict(rastbrick, gm, na.rm=T, type = "response");  names(gm.prediction) <- trainvals[i]
# writeRaster(gm.prediction, paste0('biomes3/',trainvals[i],'.tif'), overwrite=T)
# plot(gm.prediction)









library(vegan)
set.seed(0)

library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(ranger)
library(vegan)
library(cluster)
library(climatools)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

biofiles <- list.files('biomes3')
biofiles <- data.frame(file = biofiles)
biofiles <- biofiles |> mutate(biogroup = stringr::str_split_fixed(file, '\\.tif$', 2)[,1])
ecopts5 <- readRDS('ecopts5.RDS')
rastbrick <- rast('rastbrick.tif')

ecopts6 <- ecopts5
biomes <- unique(ecopts5$altbiom2023) 

Species <- c('GH','TH','GC','GM','SX','SM','TBE','TNE','TDC','TDD')

for(i in 1:length(Species)){
  x <- rast(paste0('biomes3/',Species[i],'.tif'))
  assign(Species[i], x)         };rm(x)

ttt <- rast(mget(Species))

secopts <- ecopts6[,colnames(ecopts6)[!colnames(ecopts6) %in% Species]]
secopts <- secopts[sample(1:nrow(secopts), size=1500),]
secopts.biomes <- ttt %>% extract(secopts)
secopts1 <- secopts |> cbind(secopts.biomes) |> st_drop_geometry()
secopts1$ID <- rownames(secopts1) |> as.numeric()
biome.spp <- secopts1[,Species]
biome.env <- secopts1[,c("Tw","Twh","Tg","Tc","Tclx","m","md","MAAT",
                        "bt","mholdridge","s","d","e","p","max3aet","slope","hydric",
                        "sealevel","clay","sand","marine","soilpH","bedrock")]
biome.dist <- vegan::vegdist(biome.spp, method = 'bray', binary = F)
x <- cbind(biome.spp,biome.env)

v1 <- c("Tw","Twh","Tg","Tc","Tclx","m","md","MAAT",
                   "bt","mholdridge","s","d","e","p","max3aet","slope","hydric",
                   "sealevel","clay","sand","marine","soilpH","bedrock")


vartable <- climatools::find.multithreshold(x, 'GC', 'TNE',v1)

ggplot(data=vartable, aes(x=infogain,y=reorder(variable, infogain)))+
  geom_bar(stat="identity")+
  scale_y_discrete(name='Variable')+
  scale_x_continuous(name=paste('Information Gain separating', vartable$class1[1], 'versus', vartable$class2[1]))

ggplot(data=vartable, aes(x=class1cor,y=reorder(variable, infogain)))+
  geom_bar(stat="identity")+
  scale_y_discrete(name='Variable')+
  scale_x_continuous(name=paste('Correlation with', vartable$class1[1], 'relative to', vartable$class2[1]))


ndim <- 4
nmds <- metaMDS(biome.spp, k=ndim)
en <- envfit(nmds, biome.env, na.rm = TRUE, choices=c(1:ndim))

scores(nmds)

pt.df <- scores(nmds, display='sites') |> as_tibble(rownames='sites') |> mutate(sites=as.numeric(sites)) |> inner_join(secopts1, by=join_by(sites==ID))
sp.df <- scores(nmds, display='species') |> as_tibble(rownames='species')
en.df <- scores(en, display='vectors')|> as_tibble(rownames='vectors')
print(en.df, n=nrow(en.df))

#clustering ----
mtx <- pt.df[,2:(ndim+1)]
dst <- dist(mtx)
htree1 <- dst |> agnes(method = 'ward')
for(i in 2:21){#i=2
  set.seed(0)
  nclust <- i
  
kc0 <- mtx |> kmeans(nclust) 
kc0 <- kc0$cluster
silscor <- cluster::silhouette(kc0, dist = dst)
msilscor0 <- mean(silscor[,3])

hc1 <- cutree(htree1, nclust)
hsilscor <- cluster::silhouette(hc1, dist = dst)
hmsilscor1 <- mean(hsilscor[,3])


scdf0 <- data.frame(nclust = nclust, 
                    kmeans = msilscor0, 
                    ward = hmsilscor1 
                    )
if(i==2){scdf <- scdf0}else{scdf <- rbind(scdf,scdf0)}
};rm(scdf0)
ggplot(scdf)+
  geom_line(aes(x=nclust, y=kmeans, color='kmeans'))+
  # geom_line(aes(x=nclust, y=kmeansraw, color='kmeansraw'))+
  geom_line(aes(x=nclust, y=ward, color='ward'))+
  # geom_line(aes(x=nclust, y=wardraw, color='wardraw'))+
  # geom_line(aes(x=nclust, y=upgma, color='upgma'))+
  # geom_line(aes(x=nclust, y=diana, color='diana'))+
  # geom_point(aes(x=nclust, y=kmeans, color='kmeans'))+
  # geom_point(aes(x=nclust, y=kmeansraw, color='kmeansraw'))+
  # geom_point(aes(x=nclust, y=ward, color='ward'))+
  # geom_point(aes(x=nclust, y=wardraw, color='wardraw'))+
  # geom_point(aes(x=nclust, y=upgma, color='upgma'))+
  # geom_point(aes(x=nclust, y=diana, color='diana'))+
  scale_y_continuous(name='mean silhouette')+
  scale_x_continuous(name='number of clusters', breaks = c(1:22), minor_breaks = NULL)+
  scale_color_manual(name='method',
                     labels =c('kmeans','ward','kmeansraw','wardraw','upgma','diana'), 
                     values =c('black','green','orange','magenta','red','blue'))
#########
#clustering raw ----
mtx <- pt.df[,Species]
dst <- vegdist(mtx)
htree1 <- dst |> agnes(method = 'ward')
for(i in 2:21){#i=2
  set.seed(0)
  nclust <- i
  
  kc0 <- mtx |> kmeans(nclust) 
  kc0 <- kc0$cluster
  silscor <- cluster::silhouette(kc0, dist = dst)
  msilscor0 <- mean(silscor[,3])
  
  hc1 <- cutree(htree1, nclust)
  hsilscor <- cluster::silhouette(hc1, dist = dst)
  hmsilscor1 <- mean(hsilscor[,3])
  
  
  scdf0 <- data.frame(nclust = nclust, 
                      kmeans = msilscor0, 
                      ward = hmsilscor1 
  )
  if(i==2){scdf <- scdf0}else{scdf <- rbind(scdf,scdf0)}
};rm(scdf0)
ggplot(scdf)+
  geom_line(aes(x=nclust, y=kmeans, color='kmeans'))+
  # geom_line(aes(x=nclust, y=kmeansraw, color='kmeansraw'))+
  geom_line(aes(x=nclust, y=ward, color='ward'))+
  # geom_line(aes(x=nclust, y=wardraw, color='wardraw'))+
  # geom_line(aes(x=nclust, y=upgma, color='upgma'))+
  # geom_line(aes(x=nclust, y=diana, color='diana'))+
  # geom_point(aes(x=nclust, y=kmeans, color='kmeans'))+
  # geom_point(aes(x=nclust, y=kmeansraw, color='kmeansraw'))+
  # geom_point(aes(x=nclust, y=ward, color='ward'))+
  # geom_point(aes(x=nclust, y=wardraw, color='wardraw'))+
  # geom_point(aes(x=nclust, y=upgma, color='upgma'))+
  # geom_point(aes(x=nclust, y=diana, color='diana'))+
  scale_y_continuous(name='mean silhouette')+
  scale_x_continuous(name='number of clusters', breaks = c(1:22), minor_breaks = NULL)+
  scale_color_manual(name='method',
                     labels =c('kmeans','ward','kmeansraw','wardraw','upgma','diana'), 
                     values =c('black','green','orange','magenta','red','blue'))
###########


nclust <- 10
set.seed(0)
kc1 <- pt.df[,Species] |> kmeans(nclust)
kc1 <- kc1$cluster
pt.df <- pt.df |> mutate(kc = as.factor(paste0('cluster',kc1)))
htree <- vegdist(pt.df[,Species], method='euclidean') |> agnes(method = 'ward')
plot(as.hclust(htree))
hc1 <- cutree(htree, nclust)
pt.df <- pt.df |> mutate(hc = as.factor(paste0('cluster',hc1)))

# clustersummary <- pt.df |> group_by(hc) |> summarise(across(.fns=mean))
groups <- unique(pt.df$hc) |> as.character()


gp <- ggplot() +
  geom_point(data=pt.df, aes(x=NMDS1,y=NMDS2, color=hc), alpha=0.5, size=2)+
  geom_point(data=sp.df, aes(x=NMDS1,y=NMDS2), color='blue')+
  geom_text(data=sp.df, aes(label=species, x=NMDS1,y=NMDS2), vjust = 0, nudge_y = 0.02, nudge_x = 0.05, color='blue')+
  geom_segment(data=en.df, aes(x=0,y=0,xend=NMDS1,yend=NMDS2), arrow = arrow(length = unit(0.03, "npc")), color='red')+
  geom_text(data=en.df, aes(label=vectors, x=NMDS1,y=NMDS2), vjust = 0, nudge_y = 0.02, nudge_x = 0.05, color='red')

gp


gp2 <- ggplot() +
  geom_point(data=pt.df, aes(x=NMDS3,y=NMDS4, color=hc), alpha=0.5, size=2)+
  geom_point(data=sp.df, aes(x=NMDS3,y=NMDS4), color='blue')+
  geom_text(data=sp.df, aes(label=species, x=NMDS3,y=NMDS4), vjust = 0, nudge_y = 0.02, nudge_x = 0.05, color='blue')+
  geom_segment(data=en.df, aes(x=0,y=0,xend=NMDS3,yend=NMDS4), arrow = arrow(length = unit(0.03, "npc")), color='red')+
  geom_text(data=en.df, aes(label=vectors, x=NMDS3,y=NMDS4), vjust = 0, nudge_y = 0.02, nudge_x = 0.05, color='red')

gp2



#model


#make species raster stack ----
Species <- c('GH','TH','GC','GM','SX','SM','TBE','TNE','TDC','TDD')

for(i in 1:length(Species)){
  x <- rast(paste0('biomes3/',Species[i],'.tif'))
  assign(Species[i], x)         };rm(x)

ttt <- rast(mget(Species))


pt.df$kc2 <- as.numeric(pt.df$kc)
pt.df <- pt.df |> group_by(kc2) |> mutate(wts = nrow(pt.df)/(length(kc2)+1)) |> ungroup()


formular <- as.formula(paste("kc2",paste(names(ttt), collapse = " + ", sep = ""), sep = " ~ "))

rf <-  ranger(formular, data = pt.df, classification = T, case.weights = pt.df$wts)
named <- 'biomeclusters10kcraw'
rf.prediction <-  predict(ttt, rf, na.rm=T);  names(rf.prediction) <- named
plot(rf.prediction)
writeRaster(rf.prediction, paste0('output/',named,'.tif'), overwrite=T)










v <- 'chm'
size = 20
seg <- 10
x <- chmdata0 |> st_drop_geometry()
dsample <- function(x, v, size){
  seg = 10
  x <- data.frame(v = x[,v])
  vmax <- max(x$v, na.rm = TRUE)
  vmin <- min(x$v, na.rm = TRUE)
  n <- nrow(x)
  ssize <- size/seg
  x$v1 <- floor((x$v-vmin)/(vmax-vmin)*(seg*(n-1))/n)
  x <- x |> group_by(v1) |> mutate(l = length(v1)) |> ungroup() 
  if(min(x$l) < 5){
    seg = 9
    ssize <- size/seg
    x$v1 <- floor((x$v-vmin)/(vmax-vmin)*(seg*(n-1))/n)
    x <- x |> group_by(v1) |> mutate(l = length(v1)) |> ungroup() 
  }
  if(min(x$l) < 5){
    seg = 7
    ssize <- size/seg
    x$v1 <- floor((x$v-vmin)/(vmax-vmin)*(seg*(n-1))/n)
    x <- x |> group_by(v1) |> mutate(l = length(v1)) |> ungroup() 
  }
  if(min(x$l) < 5){
    seg = 5
    ssize <- size/seg
    x$v1 <- floor((x$v-vmin)/(vmax-vmin)*(seg*(n-1))/n)
    x <- x |> group_by(v1) |> mutate(l = length(v1)) |> ungroup() 
  }
  cases <- unique(x$v1)
  for(i in 1:length(cases)){
    b <- which(x$v1 %in% cases[i])
    s0 <- sample(b, size=ssize, replace = TRUE)
    if(i==1){s=s0}else{s=c(s,s0)}
  }
  return(s)}



#CHM

chmdata <- chm %>% extract(ecopts5)
chmdata <- cbind(ecopts5, chmdata)
chmdata <- chmdata |> mutate(chm =  ifelse(is.na(chm) & 
                    ((altbiom2023 %in% c(7.3, 7.2, 7.1) & hydric < 5 & m < 0.2) | (altbiom2023 %in% c(1.1,1.2,1.3) & Tg < 3)), 0,chm))

chmdata0 <- subset(chmdata, !is.na(chm)) |> mutate(chm=chm/55)
selected <- dsample(x=st_drop_geometry(chmdata0), v='chm',size=5000)
chmdata0 <- chmdata0[selected,]

smoothvars <- c('Tw', 'Twh', 'Tg', 'Tc', 'Tclx', 'm', 'max3aet', 'md', 
                'slope', 'hydric', 'sealevel', 'clay', 'sand', 'soilpH', 'bedrock')


formular.gam <- as.formula(paste(paste("chm",paste(paste("s(",smoothvars,")", collapse = " + ", sep = ""),""), sep = " ~ ")
))

gm <- gam(formular.gam,
          family='binomial',
          data=chmdata0)
summary(gm)

TimeA <- Sys.time()
gm.prediction <-  predict(rastbrick, gm, na.rm=T, type = "response")*55;  names(gm.prediction) <- 'chm'
writeRaster(gm.prediction, paste0('biomes2/','gamchmdsample','.tif'), overwrite=T)
Sys.time() - TimeA


chmdata0 <- chmdata0 |> mutate(mtrans = m/(m+1))
chmdata0 <- chmdata0 |> mutate(fake = pmin(1,pmin(mtrans,ifelse(Tg < 10, Tg/10,1),ifelse(Tc < -10,0.5,ifelse(Tc > 0, 1, (Tc--10)/20)),(slope/(slope+5)*0.5+0.5)))) |> subset(!is.na(fake))

chmdata0$var <- chmdata0$Tg
gm <- gam(chm ~ s(var),
          family='binomial',
          data=chmdata0)
summary(gm)

ecox <-  chmdata0 |> arrange(var)
ecox <- ecox |> mutate(pre =  predict(gm, ecox, na.rm=T, type = "response"))

plot(ecox$chm ~ ecox$var); lines(ecox$pre ~ ecox$var, col='red')

formular.gam <- as.formula(paste(paste("fake",paste(paste("s(",smoothvars,")", collapse = " + ", sep = ""),""), sep = " ~ ")
)) 

gm <- gam(formular.gam,
          family='binomial',
          data=chmdata0)
summary(gm)

gm.prediction <-  predict(rastbrick, gm, na.rm=T, type = "response")*100;  names(gm.prediction) <- 'chm'
writeRaster(gm.prediction, paste0('biomes2/','fakechm','.tif'), overwrite=T)

formular.rf <- as.formula(paste(paste("fake",paste(paste(smoothvars, collapse = " + ", sep = ""),""), sep = " ~ ")
))


rf <- ranger(formular.rf,

             data=as.data.frame(chmdata0))
rf.prediction <-  predict(rastbrick, rf, na.rm=T)*100;  names(rf.prediction) <- 'chm'
writeRaster(rf.prediction, paste0('biomes2/','fakechmrf','.tif'), overwrite=T)


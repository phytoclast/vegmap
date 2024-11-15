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
md <- (e-d)/(e+0.0001); names(md)<-'md'; #writeRaster(md,paste0(path, 'md.tif'))
mholdridge <- p/(bt*58.93+0.001); names(mholdridge) <- 'mholdridge'
rastbrick <- c(Tw,Twh,Tg,Tc,Tclx,m, md, bt, mholdridge, s,d,e,p,MAAT, p3AET,slope, hydric, elev, sealevel,clay, sand,marine,soilpH,bedrock)

# pcabrick <- rastbrick[[c('Tw', 'Twh', 'Tg', 'Tc', 'Tclx', 'm', 'p3AET', 'md', 
#                          'slope', 'hydric', 'sealevel', 'clay', 'sand', 'soilpH', 'bedrock')]]
# pcabrick <- terra::princomp(pcabrick)

eco <- st_read('data/ecoregions.shp')
ecoalt <- read.csv('data/wwfeco.types.cover2.csv')
biome2023 <- read.csv('data/biome2023.csv',  na.strings = FALSE, fileEncoding = 'latin1') |> subset(select=c(ECO_ID, altbiom2023)) |> unique()
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
ecopts.repro <- ecopts.repro %>% st_as_sf() %>%  left_join(biome2023) |>  subset(select='altbiom2023')
ecopts.repro2 <- ecopts %>% project(Tw)
ecopts.repro2 <- ecopts.repro2 %>% st_as_sf() %>%  left_join(ecoalt) |>  subset(select=c('GC','GM','SX','SM','TBE','TNE','TDC','TDD')) |> st_drop_geometry()


ecopts1 <- rastbrick %>% extract(ecopts.repro)
ecopts2 <- cbind(ecopts.repro2,ecopts.repro) #%>% st_drop_geometry()
ecopts4 <- cbind(ecopts2, ecopts1)
ecopts4 <- subset(ecopts4, select=c(altbiom2023,GC,GM,SX,SM,TBE,TNE,TDC,TDD,Tw,Twh,Tg,Tc,Tclx,m, md, MAAT, bt, mholdridge, s,d,e,p, p3AET,
                                    slope,hydric,sealevel,clay,sand,marine,soilpH,bedrock))


ecopts5 <- ecopts4
ecopts5 <-  subset(ecopts5, !is.na(altbiom2023) & !is.na(Tw) & !is.na(m) & !is.na(Tclx) & !is.na(Tg) &!is.na(p3AET) &!is.na(slope) &!is.na(hydric) &!is.na(sand))


ecopts5 <-  subset(ecopts5, !(Tg > 9 & altbiom2023 %in% c(1.1,1.2,1.3))
                   & !(Tg < 3 & !altbiom2023 %in% c(1.1,1.2,1.3))
                   & !(m > 1 & altbiom2023 %in% c(7.1,7.2,7.3))
                   & !(m < 0.5 & altbiom2023 %in% c(2.1,2.2,3.1,4.1,4.2,4.3,9.1))
                   & !(Tclx < -2 & altbiom2023 %in% c(9.1))
                   & !(Tclx > 5 & altbiom2023 %in% c(1.1,1.2,2.1,2.2,3.1)))

saveRDS(ecopts5, 'ecopts5.RDS')

writeRaster(rastbrick, 'rastbrick.tif', overwrite =T)

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
rastbrick <- rast('rastbrick.tif')
ecopts6 <- ecopts5
smoothvars <- c('Tw', 'Twh', 'Tg', 'Tc', 'Tclx', 'm', 'p3AET', 'md', 
                'slope', 'hydric', 'sealevel', 'clay', 'sand', 'soilpH', 'bedrock')

formular.gam <- as.formula(paste(paste("pos",paste(paste("s(",smoothvars,")", collapse = " + ", sep = ""),""), sep = " ~ ")
))
scope.gam <- paste("~",paste("s(",smoothvars,")", collapse = " + ", sep = ""))
formular.scope.gam <- as.formula(paste("pos",paste(smoothvars, collapse = " + ", sep = ""), sep = " ~ "))


trainvals <- c('GC','GM','SX','SM','TBE','TNE','TDC','TDD')
for(i in 1:length(trainvals)){#i=3
ecopts6$pos <- ecopts6[,trainvals[i]]/100
eco <- ecopts6 |> subset(!is.na(pos))
ecopos <- eco |> subset(pos >= mean(pos))
econeg <- eco |> subset(pos < mean(pos))
ecopos <- ecopos[sample(1:nrow(ecopos), size=2200, replace=T),] 
econeg <- econeg[sample(1:nrow(econeg), size=2200, replace=T),]
takeout.p <- sample(1:nrow(ecopos), size=200, replace=F)
takeout.n <- sample(1:nrow(econeg), size=200, replace=F)
trainpos <- ecopos[-takeout.p,] 
trainneg <- econeg[-takeout.n,] 
testpos <- ecopos[takeout.p,] 
testneg <- econeg[takeout.n,] 
train <- rbind(trainpos, trainneg)
test <- rbind(testpos, testneg)




#determine optimal formulas ----
intercept_only <- gam(pos ~ 1, data=train)

scopelist <- gam.scope(train[,c('pos',smoothvars)], response = 1, smoother = "s", arg = c("df=2","df=4"), form = TRUE)
#define model with all predictors

#perform forward stepwise regression
forward <- step.Gam(intercept_only, direction='both', scope=scopelist, trace=1)
forward$formula

gm <- gam(forward$formula,
             family='binomial',
             data=train)
summary(gm)

test.gam <- test |> mutate(prediction = predict(gm, test, na.rm=T, type = "response"))
Metrics::rmse(actual=test.gam$pos, predicted=test.gam$prediction)

# gm <- gam(formular.gam,
#           family='binomial',
#           data=train)
# summary(gm)
# test.gam <- test |> mutate(prediction = predict(gm, test, na.rm=T, type = "response"))
# Metrics::rmse(actual=test.gam$pos, predicted=test.gam$prediction)
gm.prediction <-  predict(rastbrick, gm, na.rm=T, type = "response");  names(gm.prediction) <- trainvals[i]
writeRaster(gm.prediction, paste0('biomes3/',trainvals[i],'.tif'), overwrite=T)
plot(gm.prediction)
}


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
rastbrick <- rast('rastbrick.tif')

ecopts6 <- ecopts5
biomes <- unique(ecopts5$altbiom2023) 

biofiles <- list.files('C:/workspace2/vegmap/all-maps-raster-geotiff')
biofiles <- data.frame(filename = biofiles)
biofiles <- biofiles |> mutate(bioname = paste0(str_split_fixed(filename, '\\.', 3)[,1],'.',
                                                str_split_fixed(filename, '\\.', 3)[,2]
                                                ))
biofiles <- subset(biofiles, grepl('\\.tif$',filename) & grepl('^T',bioname) & !grepl('F',bioname) 
                   & !grepl('^T7',bioname)& !grepl('^T3.4',bioname)& !grepl('^T6.1',bioname) & !grepl('^T6.2',bioname) )
biofiles <- biofiles |> mutate(biomenum = as.numeric(substr(bioname, 2,4)))
# for(i in 1:nrow(biofiles)){#i=1
#   x <- rast(paste0('all-maps-raster-geotiff/',biofiles$filename[i]))
#   x <- project(x, rastbrick, method = 'near')
#   names(x) <- biofiles$bioname[i]
#   assign(biofiles$bioname[i], x)
# };rm(x)
# 
# 
# newbiome <- rast(mget(biofiles$bioname))
# writeRaster(newbiome,'all-maps-raster-geotiff/newbiome.tif')
newbiome <- rast('all-maps-raster-geotiff/newbiome.tif')
ecopts7 <-  extract(newbiome, ecopts6) |> cbind(ecopts6)

names(ecopts7[,2:35]) 
simpfun <- function(x) {
  x = case_when(is.na(x) ~ 0,
                x == 1 ~ 1,
                TRUE ~ 0.1)
}
  ecopts7 <- ecopts7 |> mutate(across(which(colnames(ecopts7)%in%biofiles$bioname),simpfun))
  ecopts7 <- ecopts7 |> mutate(T2.1 = ifelse(Tg < 3, 0,T2.1), 
                               T2.2 = ifelse(Tg < 9 ,0,T2.2),
                               T2.2 = ifelse(m < 0.5 ,0,T2.2),
                               T2.2 = T2.2^0.5,
                               T3.1 = T3.1^2,
                               T3.2 = T3.2^2,
                               T3.3 = T3.3^2,
                               T6.3 = ifelse(Tg >= 9 ,0,T6.3),
                               T6.4 = ifelse(Tg >= 9 ,0,T6.4),
                               T6.5 = ifelse(Tg >= 9 ,0,T6.5))

# biomes <- biomes |> subset(biomes %in% c(7.3));i = 1
  for (i in 1:length(biofiles$bioname)){
    # for (i in 1:length(biomes)){
      
    eco0 <- ecopts7 |> mutate(wts = ecopts7[,biofiles$bioname[i]]) |> st_drop_geometry()
   ecopos <- eco0 |> mutate(biome = biofiles$biomenum[i], pos = 1, wts = wts)
  econeg <- eco0 |> mutate(pos = 0, wts = 1-wts)
  ecopos <- ecopos[sample(1:nrow(ecopos), size=1200, replace=T, prob = ecopos$wts),] 
  econeg <- econeg[sample(1:nrow(econeg), size=1200, replace=T, prob = econeg$wts),]
  takeout.p <- sample(1:nrow(ecopos), size=200, replace=F)
  takeout.n <- sample(1:nrow(econeg), size=200, replace=F)
  trainpos <- ecopos[-takeout.p,] 
  trainneg <- econeg[-takeout.n,] 
  testpos <- ecopos[takeout.p,] 
  testneg <- econeg[takeout.n,] 
  # train <- rbind(trainpos, trainneg)
  # test <- rbind(testpos, testneg)
  if(i==1){train <- trainpos}else{train <- rbind(train,trainpos)}
  }
  
  smoothvars <- c('Tw', 'Twh', 'Tg', 'Tc', 'Tclx', 'm', 'p3AET', 'md', 
                  'slope', 'hydric', 'sealevel', 'clay', 'sand', 'soilpH', 'bedrock')


  # climvars <- c('Tw', 'Twh', 'Tg', 'Tc', 'Tclx', 'm', 'p3AET', 'md')
  # soilvars <- c('slope', 'hydric', 'sealevel', 'clay', 'sand', 'soilpH', 'bedrock')
  
  
  # formular.gam <- as.formula(paste(paste("pos",paste(paste("s(",smoothvars,")", collapse = " + ", sep = ""),""), sep = " ~ ")
  # ))
  
  # formular.glm <- as.formula(paste(paste("pos",paste(paste("I(",smoothvars,"^2)", collapse = " + ", sep = ""),""), sep = " ~ ")
  # ))
  # 
  formular.rf <- as.formula(paste(paste("biome",paste(paste(smoothvars, collapse = " + ", sep = ""),""), sep = " ~ ")
  ))

  
  rf <- ranger(formular.rf,

               data=train, classification = TRUE)
  # 
  # gm <- gam(formular.gam,
  #              family='binomial',
  #              data=train)
  # summary(gm)
  # gl <- glm(formular.glm,
  #           family='binomial',
  #           data=train)
  # summary(gl)
  # 
  # mxnt <- maxnet(p=train$pos, data= train[,smoothvars])
  # 

  # train.gam <- train |> mutate(prediction = predict(gm, train, na.rm=T, type = "response"))
  # test.gam <- test |> mutate(prediction = predict(gm, test, na.rm=T, type = "response"))
  # train.glm <- train |> mutate(prediction = predict(gl, train, na.rm=T, type = "response"))
  # test.glm <- test |> mutate(prediction = predict(gl, test, na.rm=T, type = "response"))
  # train.rf <- train |> mutate(prediction = predictions(predict(rf, train, na.rm=T)))
  # test.rf <- test |> mutate(prediction = predictions(predict(rf, test, na.rm=T)))
  # train.mxnt <- train |> mutate(prediction = predict(mxnt, as.data.frame(train), na.rm=T, type='logistic'))
  # test.mxnt <- test |> mutate(prediction = predict(mxnt, as.data.frame(test), na.rm=T, type='logistic'))
  # 
  # modmets <- data.frame(model = c("GAM", "RF","MAXNET","GLM"),
  #                       AUCtrain = c(Metrics::auc(actual=train.gam$pos, predicted=train.gam$prediction),
  #                                    Metrics::auc(actual=train.rf$pos, predicted=train.rf$prediction),
  #                                    Metrics::auc(actual=train.mxnt$pos, predicted=train.mxnt$prediction),
  #                                    Metrics::auc(actual=train.glm$pos, predicted=train.glm$prediction)),
  #                       AUCtest = c(Metrics::auc(actual=test.gam$pos, predicted=test.gam$prediction),
  #                                   Metrics::auc(actual=test.rf$pos, predicted=test.rf$prediction),
  #                                   Metrics::auc(actual=test.mxnt$pos, predicted=test.mxnt$prediction),
  #                                   Metrics::auc(actual=test.glm$pos, predicted=test.glm$prediction)),
  #                       maxKappatrain = c(maxKappa(actual=train.gam$pos, predicted=train.gam$prediction),
  #                                         maxKappa(actual=train.rf$pos, predicted=train.rf$prediction),
  #                                         maxKappa(actual=train.mxnt$pos, predicted=train.mxnt$prediction),
  #                                         maxKappa(actual=train.glm$pos, predicted=train.glm$prediction)),
  #                       maxKappatest = c(maxKappa(actual=test.gam$pos, predicted=test.gam$prediction),
  #                                        maxKappa(actual=test.rf$pos, predicted=test.rf$prediction),
  #                                        maxKappa(actual=test.mxnt$pos, predicted=test.mxnt$prediction),
  #                                        maxKappa(actual=test.glm$pos, predicted=test.glm$prediction)))
  # 
  # 
  # TimeA <- Sys.time()
  # gl.prediction <-  predict(rastbrick, gl, na.rm=T, type = "response");  names(gl.prediction) <- paste0('biome',biomes[i])
  # writeRaster(gl.prediction, paste0('biomes3/','biome',biomes[i],'_glm_fake.tif'), overwrite=T)
  # Sys.time() - TimeA
  # 
  TimeA <- Sys.time()
  # gm.prediction <-  predict(rastbrick, gm, na.rm=T, type = "response");  names(gm.prediction) <- paste0(biofiles$bioname[i])
  # writeRaster(gm.prediction, paste0('biomes3/','biome',biofiles$bioname[i],'.tif'), overwrite=T)
  # gm.prediction <-  predict(rastbrick, gm, na.rm=T, type = "response");  names(gm.prediction) <- paste0('biome',biomes[i])
  # writeRaster(gm.prediction, paste0('biomes3/','biome',biomes[i],'.tif'), overwrite=T)
  Sys.time() - TimeA
  # plot(gm.prediction)
  # TimeA <- Sys.time()
  rf.prediction <-  predict(rastbrick, rf, na.rm=T);  names(rf.prediction) <- 'biomes'
  writeRaster(rf.prediction, paste0('biomes3/','functionalbiomes.tif'), overwrite=T)
  plot(rf.prediction)
  # Sys.time() - TimeA
  # 
  # TimeA <- Sys.time()
  # mxnt.prediction <-  predict(rastbrick, mxnt, na.rm=T, type='logistic');  names(mxnt.prediction) <- paste0('biome',biomes[i])
  # writeRaster(mxnt.prediction, paste0('biomes3/','biome',biomes[i],'_mxnt_fake.tif'), overwrite=T)
  # Sys.time() - TimeA
 
TimeA <- Sys.time()
eco0$var <- eco0$Tg
  gm <- glm(pos ~ I(var^2),
            family='binomial',
            data=eco0)
  summary(gm)
Sys.time() - TimeA
    ecox <-  eco0 |> arrange(var)
    ecox <- ecox |> mutate(pre =  predict(gm, ecox, na.rm=T, type = "response"))

  plot(ecox$pos ~ ecox$var); lines(ecox$pre ~ ecox$var, col='red')


# TimeA <- Sys.time()
# Sys.time() - TimeA




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


vmax = max(v1)
vmin = min(v1)
thr <- .85*(vmax-vmin)+vmin
df <- data.frame(v1=v1,g1=g2,g2=g1)
df <- df |> mutate(grp = ifelse(v1 >= thr,1,0), g1 = g1/mean(g1),g2 = g2/mean(g2))
dsum <- df |> group_by(grp) |> summarise(g1 = mean(g1), g2=mean(g2)) |> as.data.frame()
a = dsum[1,2]
d = dsum[2,3]
c = dsum[2,2]
b = dsum[1,3]
po <- (a+d)/(a+b+c+d)
py <- ((a+b)/(a+b+c+d))*((a+c)/(a+b+c+d))
pn <- ((c+d)/(a+b+c+d))*((b+d)/(a+b+c+d))
pe = py+pn
k = (po-pe)/(1-pe)
if(k < 0){
  a = dsum[2,2]
  d = dsum[1,3]
  c = dsum[1,2]
  b = dsum[2,3]
  po <- (a+d)/(a+b+c+d)
  py <- ((a+b)/(a+b+c+d))*((a+c)/(a+b+c+d))
  pn <- ((c+d)/(a+b+c+d))*((b+d)/(a+b+c+d))
  pe = py+pn
  k = (po-pe)/(1-pe)
}
k
thr



ecopts5 <- readRDS('ecopts5.RDS')
rastbrick <- rast('rastbrick.tif')

ecopts6 <- ecopts5
biomes <- unique(ecopts5$altbiom2023) 

Species <- c("biome1.1","biome2.1","biome2.2","biome6.1","biome4.1","biome4.2","biome3.1","biome6.2","biome1.2",
             "biome7.1","biome5.2","biome5.1","biome7.2","biome7.3","biome4.3","biome8.2",
             "biome8.1","biome9.1","biome1.3","biome5.3")
for(i in 1:length(Species)){
  x <- rast(paste0('biomes2/',Species[i],'.tif'))
  assign(Species[i], x)         };rm(x)

ttt <- rast(mget(Species))


secopts <- ecopts6[sample(1:nrow(ecopts6), size=1500),]
secopts.biomes <- ttt %>% extract(secopts)
secopts1 <- secopts |> cbind(secopts.biomes) |> st_drop_geometry()
secopts1$ID <- rownames(secopts1) |> as.numeric()
Species <- c("biome1.1","biome2.1","biome2.2","biome6.1","biome4.1","biome4.2","biome3.1","biome6.2","biome1.2",
             "biome7.1","biome5.2","biome5.1","biome7.2","biome7.3","biome4.3","biome8.2",
             "biome8.1","biome9.1","biome1.3","biome5.3")
biome.spp <- secopts1[,Species]
biome.env <- secopts1[,c("Tw","Twh","Tg","Tc","Tclx","m","md","MAAT",
                        "bt","mholdridge","s","d","e","p","p3AET","slope","hydric",
                        "sealevel","clay","sand","marine","soilpH","bedrock")]
biome.dist <- vegan::vegdist(biome.spp, method = 'bray', binary = F)
x <- cbind(biome.spp,biome.env)

v1 <- c("Tw","Twh","Tg","Tc","Tclx","m","md","MAAT",
                   "bt","mholdridge","s","d","e","p","p3AET","slope","hydric",
                   "sealevel","clay","sand","marine","soilpH","bedrock")

x <- x |> mutate(maple = biome3.1/sum(biome3.1)*100, laurel = biome4.1/sum(biome4.1)*100)
x <- x |> mutate(boreal = biome2.1/sum(biome2.1)*100)
x <- x |> mutate(chaparral = biome5.1/sum(biome5.1)*100)
x <- x |> mutate(savanna = biome8.2/sum(biome8.2)*100)
x <- x |> mutate(prairie = biome6.1/sum(biome6.1)*100)
x <- x |> mutate(steppe = biome6.2/sum(biome6.2)*100)
x <- x |> mutate(sage = biome5.2/sum(biome5.2)*100)
x <- x |> mutate(spruce = (biome2.1+biome2.2)/sum(biome2.1+biome2.2)*100)
x <- x |> mutate(cloud = biome4.3/sum(biome4.3)*100, selva = biome9.1/sum(biome9.1)*100, oceanic = biome4.2/sum(biome4.2)*100)
x <- x |> mutate(tropical = (cloud+selva)/sum(cloud+selva)*100, temperate = (maple+oceanic+laurel)/sum(maple+oceanic+laurel)*100)
x <- x |> mutate(microthermic = maple, mesothermic = (oceanic+laurel)/sum(oceanic+laurel)*100)

vartable <- climatools::find.multithreshold(x, 'chaparral', 'biome5.3',v1)

ggplot(data=vartable, aes(x=infogain,y=reorder(variable, infogain)))+
  geom_bar(stat="identity")+
  scale_y_discrete(name='Variable')+
  scale_x_continuous(name=paste('Information Gain separating', vartable$class1[1], 'versus', vartable$class2[1]))

ggplot(data=vartable, aes(x=class1cor,y=reorder(variable, infogain)))+
  geom_bar(stat="identity")+
  scale_y_discrete(name='Variable')+
  scale_x_continuous(name=paste('Correlation with', vartable$class1[1], 'relative to', vartable$class2[1]))


vartable2 <- climatools::find.multithreshold(x, 'biome4.1', 'biome9.1',v1)

vartable3 <- climatools::find.multithreshold(x, 'biome4.3', 'biome9.1',v1)

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



nclust <- 5
set.seed(0)
kc1 <- pt.df[,2:(ndim+1)] |> kmeans(nclust)
kc1 <- kc1$cluster
pt.df <- pt.df |> mutate(kc = as.factor(paste0('cluster',kc1)))
htree <- vegdist(pt.df[,2:(ndim+1)], method='euclidean') |> agnes(method = 'ward')
plot(as.hclust(htree))
hc1 <- cutree(htree, nclust)
pt.df <- pt.df |> mutate(hc = as.factor(paste0('cluster',hc1)))

clustersummary <- pt.df |> group_by(kc) |> summarise(across(.fns=mean))
groups <- unique(pt.df$kc) |> as.character()
clustercorr <- pt.df
for(i in 1:length(groups)){
  clustercorr <- clustercorr |> mutate(x = ifelse(kc %in% groups[i],1,0))
  colnames(clustercorr)[colnames(clustercorr) %in% 'x'] <- groups[i]
}
clustercorr <- clustercorr |> select_if(is.numeric) |>  cor(use = 'pairwise.complete.obs') |> as.data.frame()
clustercorr <- clustercorr[,(ncol(clustercorr)-nclust+1):ncol(clustercorr)]

clusttrans <- t(clustersummary)
colnames(clusttrans) <- clustersummary$kc
clusttrans <- clusttrans[rownames(clusttrans) %in% names(Species),] |> as.data.frame()
clusttrans <- clusttrans |> mutate(across(.fns = as.numeric))
clusttrans <- clusttrans |> mutate(across(1:5,\(x).fns = round(x, 3)))

gp <- ggplot() +
  geom_point(data=pt.df, aes(x=NMDS1,y=NMDS2, color=kc), alpha=0.5, size=2)+
  geom_point(data=sp.df, aes(x=NMDS1,y=NMDS2), color='blue')+
  geom_text(data=sp.df, aes(label=species, x=NMDS1,y=NMDS2), vjust = 0, nudge_y = 0.02, nudge_x = 0.05, color='blue')+
  geom_segment(data=en.df, aes(x=0,y=0,xend=NMDS1,yend=NMDS2), arrow = arrow(length = unit(0.03, "npc")), color='red')+
  geom_text(data=en.df, aes(label=vectors, x=NMDS1,y=NMDS2), vjust = 0, nudge_y = 0.02, nudge_x = 0.05, color='red')

gp


gp2 <- ggplot() +
  geom_point(data=pt.df, aes(x=NMDS3,y=NMDS4, color=kc), alpha=0.5, size=2)+
  geom_point(data=sp.df, aes(x=NMDS3,y=NMDS4), color='blue')+
  geom_text(data=sp.df, aes(label=species, x=NMDS3,y=NMDS4), vjust = 0, nudge_y = 0.02, nudge_x = 0.05, color='blue')+
  geom_segment(data=en.df, aes(x=0,y=0,xend=NMDS3,yend=NMDS4), arrow = arrow(length = unit(0.03, "npc")), color='red')+
  geom_text(data=en.df, aes(label=vectors, x=NMDS3,y=NMDS4), vjust = 0, nudge_y = 0.02, nudge_x = 0.05, color='red')

gp2



#model


#make species raster stack ----
Species <- c("biome1.1","biome2.1","biome2.2","biome6.1","biome4.1","biome4.2","biome3.1","biome6.2","biome1.2",
             "biome7.1","biome5.2","biome5.1","biome7.2","biome7.3","biome4.3","biome8.2",
             "biome8.1","biome9.1","biome1.3","biome5.3")
for(i in 1:length(Species)){
  x <- rast(paste0('biomes2/',Species[i],'.tif'))
  assign(Species[i], x)         };rm(x)

ttt <- rast(mget(Species))


pt.df$kc2 <- as.numeric(pt.df$kc)
pt.df <- pt.df |> group_by(kc2) |> mutate(wts = nrow(pt.df)/(length(kc2)+1)) |> ungroup()


formular <- as.formula(paste("kc2",paste(names(ttt), collapse = " + ", sep = ""), sep = " ~ "))

rf <-  ranger(formular, data = pt.df, classification = T, case.weights = pt.df$wts)
named <- 'biomeclusters5'
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

smoothvars <- c('Tw', 'Twh', 'Tg', 'Tc', 'Tclx', 'm', 'p3AET', 'md', 
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


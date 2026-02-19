library(ranger)
library(gam)
library(Metrics)
library(sf)
library(terra)
library(vegnasis)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rstack <- rast("rstack25.tif")
pt.df <- readRDS('pt.df.RDS')
pt.df <- pt.df |> st_transform(crs = crs(rstack))




xy <- rstack$demsmth |> aggregate(13, na.rm = T) |> as.data.frame(xy=T)
xy2 <- xy |> mutate(ax1 = vegnasis::rotate(x,y, 0)[,1],
                    ay1 = vegnasis::rotate(x,y, 0)[,2],
                    ax2 = vegnasis::rotate(x,y, 22.5)[,1],
                    ay2 = vegnasis::rotate(x,y, 22.5)[,2],
                    ax3 = vegnasis::rotate(x,y, 45)[,1],
                    ay3 = vegnasis::rotate(x,y, 45)[,2],
                    ax4 = vegnasis::rotate(x,y, 67.5)[,1],
                    ay4 = vegnasis::rotate(x,y, 67.5)[,2])
ax1 <- rast(xy2[,c('x','y','ax1')], type='xyz', crs=crs(rstack)) 
ay1 <- rast(xy2[,c('x','y','ay1')], type='xyz', crs=crs(rstack)) 
ax2 <- rast(xy2[,c('x','y','ax2')], type='xyz', crs=crs(rstack)) 
ay2 <- rast(xy2[,c('x','y','ay2')], type='xyz', crs=crs(rstack)) 
ax3 <- rast(xy2[,c('x','y','ax3')], type='xyz', crs=crs(rstack)) 
ay3 <- rast(xy2[,c('x','y','ay3')], type='xyz', crs=crs(rstack)) 
ax4 <- rast(xy2[,c('x','y','ax4')], type='xyz', crs=crs(rstack)) 
ay4 <- rast(xy2[,c('x','y','ay4')], type='xyz', crs=crs(rstack)) 
ax1a <- rast(xy2[,c('x','y','ax1')], type='xyz', crs=crs(rstack));names(ax1a) <- 'ax1a'
ay1a <- rast(xy2[,c('x','y','ay1')], type='xyz', crs=crs(rstack));names(ay1a) <- 'ay1a' 
ax2a <- rast(xy2[,c('x','y','ax2')], type='xyz', crs=crs(rstack));names(ax2a) <- 'ax2a' 
ay2a <- rast(xy2[,c('x','y','ay2')], type='xyz', crs=crs(rstack));names(ay2a) <- 'ay2a' 
ax3a <- rast(xy2[,c('x','y','ax3')], type='xyz', crs=crs(rstack));names(ax3a) <- 'ax3a' 
ay3a <- rast(xy2[,c('x','y','ay3')], type='xyz', crs=crs(rstack));names(ay3a) <- 'ay3a' 
ax4a <- rast(xy2[,c('x','y','ax4')], type='xyz', crs=crs(rstack));names(ax4a) <- 'ax4a' 
ay4a <- rast(xy2[,c('x','y','ay4')], type='xyz', crs=crs(rstack));names(ay4a) <- 'ay4a' 
ax1b <- rast(xy2[,c('x','y','ax1')], type='xyz', crs=crs(rstack));names(ax1b) <- 'ax1b'
ay1b <- rast(xy2[,c('x','y','ay1')], type='xyz', crs=crs(rstack));names(ay1b) <- 'ay1b' 
ax2b <- rast(xy2[,c('x','y','ax2')], type='xyz', crs=crs(rstack));names(ax2b) <- 'ax2b' 
ay2b <- rast(xy2[,c('x','y','ay2')], type='xyz', crs=crs(rstack));names(ay2b) <- 'ay2b' 
ax3b <- rast(xy2[,c('x','y','ax3')], type='xyz', crs=crs(rstack));names(ax3b) <- 'ax3b' 
ay3b <- rast(xy2[,c('x','y','ay3')], type='xyz', crs=crs(rstack));names(ay3b) <- 'ay3b' 
ax4b <- rast(xy2[,c('x','y','ax4')], type='xyz', crs=crs(rstack));names(ax4b) <- 'ax4b' 
ay4b <- rast(xy2[,c('x','y','ay4')], type='xyz', crs=crs(rstack));names(ay4b) <- 'ay4b' 
axy <- c(ax1,ay1,ax2,ay2,ax3,ay3,ax4,ay4,ax1a,ay1a,ax2a,ay2a,ax3a,ay3a,ax4a,ay4a,
         ax1b,ay1b,ax2b,ay2b,ax3b,ay3b,ax4b,ay4b) 
axy <- axy |> project(rstack)
#addnoise
noise <- rast(nrow=nrow(axy), ncol=ncol(axy), ext=ext(axy), crs=crs(axy))
for(i in 1:nlyr(axy)){
  noise[] <- rnorm(ncell(axy), mean=0,sd=0)
  axy[[i]] <- axy[[i]]+noise
}


rstack2 <- c(rstack, axy)

pt.df <- pt.df |> mutate(soilclass2 = as.factor(soilclass2), textsurf = as.factor(textsurf),
                         textdeep = as.factor(textdeep))


covs <- extract(rstack2, vect(pt.df))
train <- pt.df |> cbind(covs) 

train$z <- ifelse(train$textdeep %in% 'mucky',1,0)
train <- train |> group_by(z) |> mutate(wts = 100/length(z)) |> ungroup()
  
vars <- c('ax1','ay1','ax2','ay2','ax3','ay3','ax4','ay4')
# 'demsmth', 'cdcl',  'slp', 'swi', 'vdh', 'vdl', 'vdm', 'vdvh', 'ptpo', 'prf', 'ntpo',
# 'ax1a','ay1a','ax2a','ay2a','ax3a','ay3a','ax4a','ay4a',
# 'ax1b','ay1b','ax2b','ay2b','ax3b','ay3b','ax4b','ay4b'',   )

formular.rf <- as.formula(paste(paste("z",paste(paste(vars, collapse = " + ", sep = ""),""), sep = " ~ ")
))


rf <- ranger(formular.rf,
             data=st_drop_geometry(train)
             ,case.weights = train$wts, num.trees = 500)

soilpredict <- predict(rstack2, rf, na.rm=T)
plot(soilpredict) 

fn <- 'all vars'
png(paste0(fn,'.png'),
    width = 700, height = 1090, units = "px")
plot(soilpredict, main=fn) 
points(vect(subset(train, z %in% 1)), col='red', cex=.5)
points(vect(subset(train, z %in% 0)), col='black', cex=.5)
dev.off()



library(nnet)
nn <- nnet(formular.rf,
           data=as.data.frame(st_drop_geometry(train))
           ,size = 2)
soilpredict <- predict(rstack2, nn, na.rm=T)
plot(soilpredict) 
formular.gam <- as.formula(paste(paste("z",paste(paste("s(",vars,")", collapse = " + ", sep = ""),""), sep = " ~ ")
))
gm <- gam(formular.gam,
                             data=as.data.frame(st_drop_geometry(train)), type='logistic')
soilpredict <- predict(rstack2, gm, na.rm=T, type='response')
plot(soilpredict) 

library(maxnet)
mxnt <- maxnet(p=train$z, data= st_drop_geometry(train[,vars]))
soilpredict <- predict(rstack2, mxnt, na.rm=T, type='logistic')
plot(soilpredict) 

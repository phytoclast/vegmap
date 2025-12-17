library(terra)
library(sf)
library(climatools)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pdata <- readRDS('ghcn/pdata.RDS')
tdata <- readRDS('ghcn/tdata2.RDS') 
trdata <- readRDS('ghcn/trdata2.RDS') 
stations.p <- readRDS('ghcn/stations.p2.RDS')
stations.t <- readRDS('ghcn/stations.t2.RDS')
stations.tr <- readRDS('ghcn/stations.tr2.RDS')
pdata <- stations.p |> left_join(pdata)
tdata <- stations.t |> left_join(tdata)
trdata <- stations.tr |> left_join(trdata)

# library(missForest)

t1  <- tdata |> st_drop_geometry()
databyyear <- subset(t1, YEAR >1950 & YEAR <=2010 & !is.na(t01) & !is.na(t07), select=c(ID, YEAR)) |> unique() |> group_by(ID) |> summarise(nYEAR = length(YEAR)) |> subset(nYEAR >= 15)

thesecols <- c("ID","NAME","y","x","elev","lat","sinlon","coslon","w50","w500","w5000","wind000",
               "wind045","wind090","wind135","wind180","wind225","wind270","wind315","YEAR",
               "t01","t02","t03","t04","t05","t06","t07","t08","t09","t10","t11","t12")
headcols <- c("ID","NAME","y","x","elev","lat","sinlon","coslon","w50","w500","w5000","wind000",
               "wind045","wind090","wind135","wind180","wind225","wind270","wind315")
datcols <- c("t01","t02","t03","t04","t05","t06","t07","t08","t09","t10","t11","t12")
numcols <- c("elev","lat","sinlon","coslon","w50","w500","w5000","wind000",
             "wind045","wind090","wind135","wind180","wind225","wind270","wind315","YEAR",
             "t01","t02","t03","t04","t05","t06","t07","t08","t09","t10","t11","t12")
ndatcols <- c("x","y","elev","lat","sinlon","coslon","w50","w500","w5000","wind000",
             "wind045","wind090","wind135","wind180","wind225","wind270","wind315","YEAR")

tdata2 <- subset(tdata,YEAR > 1950 & YEAR <=2010 & ID %in% databyyear$ID, select=thesecols) |> st_drop_geometry() 
dfhead <- subset(tdata2,select=headcols) |> unique()
blanks <- data.frame(YEAR=1961:1990,t01=NA,t02=NA,t03=NA,t04=NA,t05=NA,t06=NA,t07=NA,t08=NA,t09=NA,t10=NA,t11=NA,t12=NA)
remNAN <- function(x){ifelse(is.nan(x), NA,x)}
blanks <- dfhead |> merge(blanks)
tdata2 <- tdata2 |> rbind(blanks)
tdata2 <- tdata2 |> group_by(ID, YEAR) |> 
  summarize(across(all_of(datcols), \(x) mean(x, na.rm = TRUE))) |> ungroup()
tdata2 <- tdata2 |> mutate(across(all_of(datcols), remNAN))
tdata2 <- dfhead |> left_join(tdata2)

(180+180)/20
(90+90)/10
tprocess <- subset(tdata2, YEAR %in% c(1961:1990) & !is.na(t01) & !is.na(t07), select=c(ID, x,y,elev)) |> unique()|> mutate(llgrp=paste('ll',floor(y/15)*15,floor(x/15)*15)) |> group_by(llgrp) |> summarise(n=length(ID), xmin=min(x), xmax=max(x), ymin=min(y), ymax=max(y)) 

#75-90 lat one zone
#60-75 90 deg lon
#45-60 30 deg lon
#-15-45 15 deg lon  west of -45lon

#test models -----
ex <- c(-90,-60,30,60)
ex <- c(-180,-150,30,60)
addthis <- c(-5,5,-5,5)
exa <- ex+addthis

degf <- 7
tprocess <- subset(tdata2, x >= exa[1] & x <= exa[2] & y >= exa[3] & y <= exa[4]) |> mutate(z = t01) |> 
  mutate(llgrp0 = paste('ll',floor(x/degf),floor(y/degf)),
         llgrp1 = paste('ll',floor((x+degf/2)/degf),floor(y/degf)),
         llgrp2 = paste('ll',floor(x/degf),floor((y+degf/2)/degf)),
         llgrp3 = paste('ll',floor((x+degf/2)/degf),floor((y+degf/2)/degf)), ycat = factor(YEAR))
train0 <- tprocess |> subset(!is.na(z))
ntest <- sample(1:nrow(train0), size = 0.1*nrow(train0))
test <- train0[ntest,]
train <- train0[-ntest,]
library(gam)
lmod = gam(z~s(x)+s(y)+s(elev)+s(YEAR)+w50+w500+w5000+
             wind000+wind090+wind180+wind270+
             wind045+wind135+wind225+wind315,data=train)
summary(lmod)
test <- test |> mutate(pred = predict(lmod, test, type='response'))
mean((test$pred - test$z)^2)^0.5
train <- train |> mutate(pred = predict(lmod, train, type='response'))
mean((train$pred - train$z)^2)^0.5


trsd <- train |> mutate(pred = predict(lmod, train, type='response'))
trsd <- trsd |> group_by(ID) |> mutate(res1 = mean(z-pred, na.rm=T))
trsd <- trsd |> group_by(llgrp0,YEAR) |> mutate(res2.0 = mean(z-(pred+res1), na.rm=T)) |>ungroup()
trsd <- trsd |> group_by(llgrp1,YEAR) |> mutate(res2.1 = mean(z-(pred+res1), na.rm=T)) |>ungroup()
trsd <- trsd |> group_by(llgrp2,YEAR) |> mutate(res2.2 = mean(z-(pred+res1), na.rm=T)) |>ungroup()
trsd <- trsd |> group_by(llgrp3,YEAR) |> mutate(res2.3 = mean(z-(pred+res1), na.rm=T)) |>ungroup()
trsd$res2 <- rowMeans(trsd[,c('res2.0','res2.1','res2.2','res2.3')],na.rm = T)
trsd <- trsd |> mutate(pred2 = pred+res1+res2)
mean((trsd$pred2 - trsd$z)^2, na.rm=T)^0.5
resids <- subset(trsd, select=c(llgrp0,llgrp1,llgrp2,llgrp3,YEAR, res1, res2)) |> unique()
trsd <- test |> left_join(resids)
trsd <- trsd |> mutate(pred  = predict(lmod, trsd, type='response'))
trsd <- trsd |> mutate(pred2 = pred+res1+res2)
mean((trsd$pred2 - trsd$z)^2, na.rm=T)^0.5

#full data----
library(gam)
ex <- c(-90,-60,30,60)
ex <- c(-180,-150,30,60)
ex <- c(-180,-150,-60,-30)
bufs <- c(5,10,20)
degf0 <- c(3,10,30,100)
allmonths <- c('t01','t02','t03','t04','t05','t06','t07','t08','t09','t10','t11','t12')
headercols <- c('ID','NAME','y','x','elev','YEAR')
precount <- subset(tdata2, x >= ex[1] & x <= ex[2] & y >= ex[3] & y <= ex[4], select=c(ID,x,y)) |> unique() |> nrow()
if(precount<=0){next}

for(i.m in 1:12){#i.m=1
thismonth <- allmonths[i.m]
for(i.b in 1:3){#i.b=1
buf <- bufs[i.b]
addthis <- c(-1*buf,buf,-1*buf,buf)
exa <- ex+addthis
tprocess <- subset(tdata2, x >= exa[1] & x <= exa[2] & y >= exa[3] & y <= exa[4]) 

tprocess$z <- unlist(tprocess[,thismonth])
count1990 <- subset(tprocess, !is.na(z) & YEAR %in% c(1961:1990)) |> group_by(ID) |> summarise(nz = length(z)) |> subset(nz >= 30) |> nrow()

if(count1990 >=3){break}}

lmod = gam(z~s(x)+s(y)+s(elev)+s(YEAR)+w50+w500+w5000+
             wind000+wind090+wind180+wind270+
             wind045+wind135+wind225+wind315,data=tprocess)
trsd <- tprocess |> mutate(pred = predict(lmod, tprocess, type='response'))
trsd <- trsd |> group_by(ID) |> mutate(res1 = mean(z-pred, na.rm=T))
trsd$pred2 <- NA
for(i.d in 1:4){#i.d=1
degf <- degf0[i.d]
trsd <- trsd |> 
  mutate(llgrp0 = paste('ll',floor(x/degf),floor(y/degf)),
         llgrp1 = paste('ll',floor((x+degf/2)/degf),floor(y/degf)),
         llgrp2 = paste('ll',floor(x/degf),floor((y+degf/2)/degf)),
         llgrp3 = paste('ll',floor((x+degf/2)/degf),floor((y+degf/2)/degf)), ycat = factor(YEAR))

trsd <- trsd |> group_by(llgrp0,YEAR) |> mutate(res2.0 = mean(z-(pred+res1), na.rm=T)) |>ungroup()
trsd <- trsd |> group_by(llgrp1,YEAR) |> mutate(res2.1 = mean(z-(pred+res1), na.rm=T)) |>ungroup()
trsd <- trsd |> group_by(llgrp2,YEAR) |> mutate(res2.2 = mean(z-(pred+res1), na.rm=T)) |>ungroup()
trsd <- trsd |> group_by(llgrp3,YEAR) |> mutate(res2.3 = mean(z-(pred+res1), na.rm=T)) |>ungroup()
trsd$res2 <- rowMeans(trsd[,c('res2.0','res2.1','res2.2','res2.3')],na.rm = T)
trsd <- trsd |> mutate(pred2 = ifelse(is.na(pred2),pred+res1+res2,pred2))
nna <- subset(trsd, is.na(pred2)) |> nrow()
if(nna <= 0){
  break
}
}
trsd <- trsd |> subset(x >= ex[1] & x <= ex[2] & y >= ex[3] & y <= ex[4]) |> as.data.frame()
thisz <- trsd$z
preds <- trsd$pred2
thisz <- ifelse(is.na(thisz),preds,thisz)
newdata0 <- trsd[,headercols]
newdata0$z <- thisz
colnames(newdata0) <- c(headercols, thismonth)
if(i.m == 1){newdata <- newdata0}else{newdata <- newdata |> left_join(newdata0)}
}



library(ggplot2)
#'USC00406328''USC00315923''USC00403420''USW00003812'
#'USW00026451  USC00500275 USW00025628
ggplot()+
  geom_point(data=subset(trsd,ID %in% 'USC00500272'), aes(x=YEAR, y=z))+
  geom_line(data=subset(trsd,ID %in% 'USC00500272'), aes(x=YEAR, y=z))+
  geom_point(data=subset(trsd,ID %in% 'USW00025628'), aes(x=YEAR, y=z), color='red')+
  geom_line(data=subset(trsd,ID %in% 'USW00025628'), aes(x=YEAR, y=z), color='red')+
  geom_point(data=subset (trsd,ID %in% 'USC00500272'), aes(x=YEAR, y=pred2), color='green')+
  geom_line(data=subset(trsd,ID %in% 'USC00500272'), aes(x=YEAR, y=pred2), color='green')+
  geom_point(data=subset(trsd,ID %in% 'USW00025628'), aes(x=YEAR, y=pred2), color='blue')+
  geom_line(data=subset(trsd,ID %in% 'USW00025628'), aes(x=YEAR, y=pred2), color='blue')



train <- train |> mutate(preds = predictions(predict(rf,train)))
micetest <- mice.mids(imp, newdata = tprocess)
mtest <- complete(micetest)

ggplot()+
  geom_point(data=subset(tprocess,ID %in% 'USC00403420'), aes(x=YEAR, y=t01))+
  geom_line(data=subset(tprocess,ID %in% 'USC00403420'), aes(x=YEAR, y=t01))+
  geom_point(data=subset(tprocess,ID %in% 'USC00406328'), aes(x=YEAR, y=t01), color='red')+
  geom_line(data=subset(tprocess,ID %in% 'USC00406328'), aes(x=YEAR, y=t01), color='red')+
  geom_point(data=subset (tprocess,ID %in% 'USC00403420'), aes(x=YEAR, y=preds), color='green')+
  geom_line(data=subset(tprocess,ID %in% 'USC00403420'), aes(x=YEAR, y=preds), color='green')+
  geom_point(data=subset(tprocess,ID %in% 'USC00406328'), aes(x=YEAR, y=preds), color='blue')+
  geom_line(data=subset(tprocess,ID %in% 'USC00406328'), aes(x=YEAR, y=preds), color='blue')+
  geom_point(data=subset(mtest,ID %in% 'USC00406328'), aes(x=YEAR, y=t01), color='orange')+
  geom_line(data=subset(mtest,ID %in% 'USC00406328'), aes(x=YEAR, y=t01), color='orange')


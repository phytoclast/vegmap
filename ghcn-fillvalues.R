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

library(missForest)

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


tprocess <- subset(tdata2, y >= 30 & y <= 50 & x >= -90 & x <= -60) |> mutate(z = t01) |> 
  mutate(llgrp = paste('ll',floor(x/5),floor(y/5)), ycat = factor(YEAR))
train0 <- tprocess |> subset(!is.na(z))
ntest <- sample(1:nrow(train0), size = 0.1*nrow(train0))
test <- train0[ntest,]
train <- train0[-ntest,]

#Linear model incorporating unique geographic (xy grouped by 5 degrees) year interactions (very time consuming, error: test 2.53, train 1.17, vs gam model without interactions at test 2.72, train 2.71, for resids grouped by station then resids grouped by geography and year train 1.34, test 0.74)
# lmod = lm(z~x+y+elev+YEAR+llgrp:ycat,data=train)
# summary(lmod)
# test <- test |> mutate(pred = predict(lmod, test))
# mean((test$pred - test$z)^2)^0.5
# train <- train |> mutate(pred = predict(lmod, train))
# mean((train$pred - train$z)^2)^0.5

library(gam)
lmod = gam(z~s(x)+s(y)+s(elev)+s(YEAR),data=train)
summary(lmod)
test <- test |> mutate(pred = predict(lmod, test, type='response'))
mean((test$pred - test$z)^2)^0.5
train <- train |> mutate(pred = predict(lmod, train, type='response'))
mean((train$pred - train$z)^2)^0.5

trsd <- train |> mutate(pred = predict(lmod, train, type='response'))
trsd <- trsd |> group_by(ID) |> mutate(res1 = mean(z-pred, na.rm=T))
trsd <- trsd |> group_by(llgrp,YEAR) |> mutate(res2 = mean(z-(pred+res1), na.rm=T)) |>ungroup()
trsd <- trsd |> mutate(pred2 = pred+res1+res2)
mean((trsd$pred2 - trsd$z)^2, na.rm=T)^0.5
resids <- subset(trsd, select=c(llgrp,YEAR, res1, res2)) |> unique()
trsd <- test |> left_join(resids)
trsd <- trsd |> mutate(pred = predict(lmod, trsd, type='response'))
trsd <- trsd |> mutate(pred2 = pred+res1+res2)
mean((trsd$pred2 - trsd$z)^2, na.rm=T)^0.5

library(ggplot2)
#'USC00406328''USC00315923''USC00403420''USW00003812'
ggplot()+
  geom_point(data=subset(trsd,ID %in% 'USC00403420'), aes(x=YEAR, y=t01))+
  geom_line(data=subset(trsd,ID %in% 'USC00403420'), aes(x=YEAR, y=t01))+
  geom_point(data=subset(trsd,ID %in% 'USC00406328'), aes(x=YEAR, y=t01), color='red')+
  geom_line(data=subset(trsd,ID %in% 'USC00406328'), aes(x=YEAR, y=t01), color='red')+
  geom_point(data=subset (trsd,ID %in% 'USC00403420'), aes(x=YEAR, y=pred), color='green')+
  geom_line(data=subset(trsd,ID %in% 'USC00403420'), aes(x=YEAR, y=pred), color='green')+
  geom_point(data=subset(trsd,ID %in% 'USC00406328'), aes(x=YEAR, y=pred), color='blue')+
  geom_line(data=subset(trsd,ID %in% 'USC00406328'), aes(x=YEAR, y=pred), color='blue')

library(mice)
imp <- mice(tprocess)

train <- train <- train |> mutate(preds = predictions(predict(rf,train)))
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


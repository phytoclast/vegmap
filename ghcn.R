library(terra)
library(sf)
library(climatools)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
chelsa <- rast('C:/scripts/vegmap/chelsa2.1/chelsa.tif')
names(chelsa)
stations <- read.fwf('ghcn/ghcnd-stations.txt', widths = c(11,9,10,7,31))
colnames(stations) <- c('ID','LATITUDE','LONGITUDE','STNELEV','NAME')            
# #stops at 93506 
# tdata <- read.fwf('ghcn/v4/ghcnm.tavg.latest.qcf/ghcnm.v4.0.1.20250405/ghcnm.tavg.v4.0.1.20250405.qcf.dat',
#                     widths = c(11,4,4,
#                                5,1,1,1,
#                                5,1,1,1,
#                                5,1,1,1,
#                                5,1,1,1,
#                                5,1,1,1,
#                                5,1,1,1,
#                                5,1,1,1,
#                                5,1,1,1,
#                                5,1,1,1,
#                                5,1,1,1,
#                                5,1,1,1,
#                                5,1,1,1), skip=93506,n=1000, fill=T)
# tdata <- read.table('ghcn/v4/ghcnm.tavg.latest.qcf/ghcnm.v4.0.1.20250405/ghcnm.tavg.v4.0.1.20250405.qcf.dat', skip=100000, nrows  =1000, fill=T, sep='|')#, 

#How to insert line breaks before or after----
# text <- "999   -999BGXLT9671401957TAVG-9999   -9999   -9999   -9999 "
# pattern <- ".{11}TAVG" # Pattern to insert line break before or after
# new_text <- gsub(paste0("(?=", pattern, ")"), "@", text, perl = TRUE)
# new_text <- gsub(paste0("(?<=", pattern, ")"), "@", text, perl = TRUE)




#import whole file
input_lines <- readLines('ghcn/v4/ghcnm.tavg.latest.qcf/ghcnm.v4.0.1.20250405/ghcnm.tavg.v4.0.1.20250405.qcf.dat')
# Collapse all lines into a single string
full_text <- paste(input_lines, collapse = "")
#search for pattern for appropriate line breaks
pattern <- ".{15}TAVG" 
#insert line break before pattern
new_text <- gsub(paste0("(?=", pattern, ")"), "\n", full_text, perl = TRUE)
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
           5,1,1,1)
tdata <- read.fwf(textConnection(new_text), widths=widths, fill = T)
# tdata <- read.fwf(textConnection(new_text), widths=widths, skip=93500, n=100, fill = T)
# tdata <- read.table(textConnection(new_text), skip=93500, nrow=100, fill = T )


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

tdata <- tdata[,c('ID','YEAR','ELEMENT',
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


tdata[,c(4:15)] <- sapply(tdata[,c(4:15)], FUN=makena )
tdata[,c(4:15)] <- tdata[,c(4:15)]/100
saveRDS(tdata, 'ghcn/tdata.RDS')
###############
library(terra)
library(sf)
library(climatools)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
stations <- read.csv('ghcn/stbr.csv')
stations <- stations |> mutate(elev = ifelse(is.na(elev), STNELEV, elev)) 
stations <- stations |> subset(elev!=-999.9)
tdata <- readRDS('ghcn/tdata.RDS')
tsdata <- stations |> left_join(tdata)
saveRDS(tsdata, 'ghcn/tsdata.RDS')

library(gam)
tsdata <- tsdata |> mutate(lat = LATITUDE, sinlon = sin(LONGITUDE/360*2*pi), coslon = cos(LONGITUDE/360*2*pi))

thisdata <- tsdata |> mutate(y=t01) |> 
  subset(!is.na(lat) &!is.na(y))
smoothvars <- c("lat","sinlon","coslon","elev","w50","w500","w5000","YEAR")

formular.gam <- as.formula(paste(paste("y",paste(paste("s(",smoothvars,")", collapse = " + ", sep = ""),""), sep = " ~ "), "+lat*coslon*sinlon*elev*YEAR"
))
formular.gam <- as.formula(paste(paste("y",paste(paste("s(",smoothvars,",8)", collapse = " + ", sep = ""),""), sep = " ~ ")
))

gm <- gam(formular.gam,
          family='gaussian',
          data=thisdata, rm.na=T)

summary(gm)

grdata <- thisdata |> subset(grepl('TN KNOX', NAME))
grdata <- thisdata |> subset(grepl('CA LOS ANG', NAME))
grdata <- thisdata |> subset(grepl('MI GRAND RA', NAME))
grdata <- grdata |> mutate(prd = predict.Gam(gm,grdata))
data1 <- subset(thisdata, lat > 30 & lat < 32 & YEAR >=2000 & YEAR <=2010 & elev >0 & elev <500)
data2 <- subset(thisdata, lat > 30 & lat < 32 & YEAR >=1920 & YEAR <=1940 & elev >0 & elev <500)
data1 <- data1 |> mutate(prd = predict.Gam(gm,data1))
data2 <- data2 |> mutate(prd = predict.Gam(gm,data2))
library(ggplot2)

ggplot()+
  geom_point(data=data1, aes(x=LONGITUDE,y=prd), color='red')+
  geom_point(data=data2, aes(x=LONGITUDE,y=prd))


data1 <- subset(thisdata, LONGITUDE > 70 & LONGITUDE < 80 & YEAR >=2000 & YEAR <=2010 & elev >0 & elev <500)
data2 <- subset(thisdata, LONGITUDE > 70 & LONGITUDE < 80 & YEAR >=1920 & YEAR <=1940 & elev >0 & elev <500)
data1 <- data1 |> mutate(prd = predict.Gam(gm,data1))
data2 <- data2 |> mutate(prd = predict.Gam(gm,data2))
library(ggplot2)

ggplot()+
  geom_point(data=data1, aes(x=LATITUDE,y=prd), color='red')+
  geom_point(data=data2, aes(x=LATITUDE,y=prd))

data1 <- thisdata |> subset(select=c("LONGITUDE",smoothvars)) |> unique()
data1 <- data1 |> mutate(YEAR = 2010, p2010 = predict.Gam(gm,data1))
data1 <- data1 |> mutate(YEAR = 1910, p1910 = predict.Gam(gm,data1))
data1 <- data1 |> mutate(warming= p2010-p1910)

datax <- subset(data1, LONGITUDE > -87 & LONGITUDE < -80 & elev >0 & elev <500)

ggplot()+
  geom_point(data=datax, aes(x=lat,y=warming), color='red')

library(gstat)
library(terra)
library(sf)
library(climatools)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
stations <- read.fwf('ghcn/ghcnd-stations.txt', widths = c(11,9,10,7,31))
stationsx <- read.fwf('ghcn/v4/ghcnm.tavg.v4.0.1.20251106.qcf.inv', widths = c(11,9,10,7,31))
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
stations1 <- read.fwf('ghcn/v3/ghcnm.tmax.v3.3.0.20190817.qca.inv', widths = c(11,9,10,7,31,5,1,5,2,2,2,2,1,2,16,1), comment.char = "")
stationsx <- read.fwf('ghcn/v3/ghcnm.tmax.v3.3.0.20190817.qcu.inv', widths = c(11,9,10,7,31,5,1,5,2,2,2,2,1,2,16,1), comment.char = "")
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
saveRDS(trdata, 'ghcn/trdata.RDS')
###############

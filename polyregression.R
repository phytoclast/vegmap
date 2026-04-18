library(terra)
library(sf)
library(climatools)
library(dplyr)
library(fields)
library(gstat)
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


elevmin <- rast('global/elevmin.tif')
br1 <- rast('global/br5000.tif')
belts <- rast('global/belts.tif')
bsns <- rast('ghcn/bsns.tif')
pchelsa <- rast('global/pchelsa.tif')
pchelsa <- pchelsa |> project(br1)
tchelsa <- rast('global/tchelsa.tif')
tchelsa <- tchelsa |> project(br1)
# belts <- belts |> project(br1)
br1 <- c(br1,belts, pchelsa, tchelsa)
br1$tropical <- br1$monsoonE+br1$monsoonW+br1$tradeN+br1$tradeS
psummer <- rast('global/psummer.tif')
trsummer <- rast('global/trsummer.tif')
tsummer <- rast('global/tsummer.tif')
pwinter <- rast('global/pwinter.tif')
trwinter <- rast('global/trwinter.tif')
twinter <- rast('global/twinter.tif')
basemap <- c(psummer,trsummer,tsummer,pwinter,trwinter,twinter, bsns)
br1 <- c(br1,basemap)
ex <- c(-180, 20, 0, 90)
ex <- c(-180, -30, 0, 90)#north america
#ex <- c(-180, -10, -90, 20)#south america
#ex <- c(-30, 180, 10, 90)#
#ex <- c(-30, 180, -90, 20)#

br2 <- br1 |> crop(ext(ex))
elev1 <- br2$elev |> crop(ext(ex))
elev10 <- elev1 |> aggregate(10)
mos <- c("01","02","03","04","05","06","07","08","09","10","11","12")


parm = 't'
mo = 1

if(parm %in% 't'){
  globl <- readRDS('ghcn/t.globl.RDS') |> subset(!ID %in% c('CA002202750', 'CA006092920', 'CA007080452', 'ACW00011604','CA002402051', 'CA002402332','CA002300551'))
  
  vars = c("coslon","sinlon","lat","relev","elev","w5000","w500","w50")
  
  formular.gm <- as.formula(paste(paste("z",paste(paste(vars, collapse = " + ", sep = ""),""), sep = " ~ ")))
}

xsr <- seq(ex[1],ex[2],10)
ysr <- seq(ex[3],ex[4],10)
cef=NULL
for(i in 1:length(xsr)){
  for(j in 1:length(ysr)){
# i=7;j=5
x0 = xsr[i]
y0 = ysr[j]

fex = c(x0-5,x0+5,y0-5,y0+5)

#select dependent variable
globl$z <- globl |> select(paste0(parm,mos[mo])) |> st_drop_geometry() |> as.vector() |> unlist()

globl <- globl |> mutate(e3n = ifelse(elev >= 2500 & y >= (fex[4]*3+fex[3])/4,1,0),
                         e3s = ifelse(elev >= 2500 & y <= (fex[3]*3+fex[4])/4,1,0),
                         e3e = ifelse(elev >= 2500 & x >= (fex[2]*3+fex[1])/4,1,0),
                         e3w = ifelse(elev >= 2500 & x <= (fex[1]*3+fex[2])/4,1,0),
                         e2n = ifelse(elev >= 1000 & elev < 2500 & y >= (fex[4]*3+fex[3])/4,1,0),
                         e2s = ifelse(elev >= 1000 & elev < 2500 & y <= (fex[3]*3+fex[4])/4,1,0),
                         e2e = ifelse(elev >= 1000 & elev < 2500 & x >= (fex[2]*3+fex[1])/4,1,0),
                         e2w = ifelse(elev >= 1000 & elev < 2500 & x <= (fex[1]*3+fex[2])/4,1,0),
                         e1n = ifelse(elev <= 200 & y >= (fex[4]*3+fex[3])/4,1,0),
                         e1s = ifelse(elev <= 200 & y <= (fex[3]*3+fex[4])/4,1,0),
                         e1e = ifelse(elev <= 200 & x >= (fex[2]*3+fex[1])/4,1,0),
                         e1w = ifelse(elev <= 200 & x <= (fex[1]*3+fex[2])/4,1,0),
                         w1n = ifelse(w50 >= 0.5 & y >= (fex[4]*3+fex[3])/4,1,0),
                         w1s = ifelse(w50 >= 0.5 & y <= (fex[3]*3+fex[4])/4,1,0),
                         w1e = ifelse(w50 >= 0.5 & x >= (fex[2]*3+fex[1])/4,1,0),
                         w1w = ifelse(w50 >= 0.5 & x <= (fex[1]*3+fex[2])/4,1,0),
                         w0n = ifelse(w500 <= 0.1 & y >= (fex[4]*3+fex[3])/4,1,0),
                         w0s = ifelse(w500 <= 0.1 & y <= (fex[3]*3+fex[4])/4,1,0),
                         w0e = ifelse(w500 <= 0.1 & x >= (fex[2]*3+fex[1])/4,1,0),
                         w0w = ifelse(w500 <= 0.1 & x <= (fex[1]*3+fex[2])/4,1,0),
                         d = (((x-(fex[1]+fex[2])/2)^2+(y-(fex[3]+fex[4])/2)^2))^0.5)


globl <- globl |> mutate(e3n = ifelse(min(d/(e3n+0.0001))== d/(e3n+0.0001),1,0),
                         e3s = ifelse(min(d/(e3s+0.0001))== d/(e3s+0.0001),1,0),
                         e3e = ifelse(min(d/(e3e+0.0001))== d/(e3e+0.0001),1,0),
                         e3w = ifelse(min(d/(e3w+0.0001))== d/(e3w+0.0001),1,0),
                         e2n = ifelse(min(d/(e2n+0.0001))== d/(e2n+0.0001),1,0),
                         e2s = ifelse(min(d/(e2s+0.0001))== d/(e2s+0.0001),1,0),
                         e2e = ifelse(min(d/(e2e+0.0001))== d/(e2e+0.0001),1,0),
                         e2w = ifelse(min(d/(e2w+0.0001))== d/(e2w+0.0001),1,0),
                         e1n = ifelse(min(d/(e1n+0.0001))== d/(e1n+0.0001),1,0),
                         e1s = ifelse(min(d/(e1s+0.0001))== d/(e1s+0.0001),1,0),
                         e1e = ifelse(min(d/(e1e+0.0001))== d/(e1e+0.0001),1,0),
                         e1w = ifelse(min(d/(e1w+0.0001))== d/(e1w+0.0001),1,0),
                         w1n = ifelse(min(d/(w1n+0.0001))== d/(w1n+0.0001),1,0),
                         w1s = ifelse(min(d/(w1s+0.0001))== d/(w1s+0.0001),1,0),
                         w1e = ifelse(min(d/(w1e+0.0001))== d/(w1e+0.0001),1,0),
                         w1w = ifelse(min(d/(w1w+0.0001))== d/(w1w+0.0001),1,0),
                         w0n = ifelse(min(d/(w0n+0.0001))== d/(w0n+0.0001),1,0),
                         w0s = ifelse(min(d/(w0s+0.0001))== d/(w0s+0.0001),1,0),
                         w0e = ifelse(min(d/(w0e+0.0001))== d/(w0e+0.0001),1,0),
                         w0w = ifelse(min(d/(w0w+0.0001))== d/(w0w+0.0001),1,0),
                         drank = rank(d))

gselect <- subset(globl, x >= fex[1] & x <= fex[2] & y >= fex[3] & y <= fex[4] |
                    e3n ==1| e3s ==1| e3e ==1| e3w ==1 |
                    e2n ==1| e2s ==1| e2e ==1| e2w ==1 |
                    e1n ==1| e1s ==1| e1e ==1| e1w ==1 |
                    w1n ==1| w1s ==1| w1e ==1| w1w ==1 |
                    w0n ==1| w0s ==1| w0e ==1| w0w ==1 | drank <= 100)

gselect <- gselect |> mutate(geotile = paste('ll',floor(x/5)*100,floor(y/5)), elzone = floor(((elev/500)+0.1)^1*1))|> group_by(geotile, elzone) |> mutate(wts=100/length(ID))

# gselect <- subset(globl, x >= fex[1] & x <= fex[2] & y >= fex[3] & y <= fex[4] |
#                     drank <= 100)

# gselect <- subset(globl, drank <= 100)



gm = glm(formular.gm
            ,data=gselect, weights = gselect$wts)

# summary(gm)
# plot(vect(globl), cex=0.01, col='black', alpha=0.05)
# points(vect(gselect), cex=0.1, col='red')

cef0 <- data.frame(t(gm$coefficients))
cef0$x = x0
cef0$y = y0
if(is.null(cef)){cef <- cef0}else{cef <- rbind(cef,cef0)}
  }}

cefv <- vect(cef, geom=c("x", "y"), crs=crs(elev10))
cefr <- rasterize(cefv, y=rast(res=c(10,10), ext=ext(cefv)), field=vars)
cefi <- rasterize(cefv, y=rast(res=c(10,10), ext=ext(cefv)), field='X.Intercept.')
cefr <- cefr |> project(br2)
cefi <- cefi |> project(br2)
plot(cefr$elev)
#plot(cefi)
points(vect(globl), cex=0.01, col='black', alpha=0.05)

preds <- sum(cefr*br2[[vars]])+cefi
plot(preds)
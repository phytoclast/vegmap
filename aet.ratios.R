

i=1
month <- c('01','02','03','04','05','06','07','08','09','10','11','12')
for(i in 1:12){
  p0 = rast(paste0('D:/scripts/processclimategrids/data/p',month[i],'.tif'))
  e0 = rast(paste0('D:/scripts/processclimategrids/output/e',month[i],'.tif'))
  names(p0) = paste0('p',month[i])
  names(e0) = paste0('e',month[i])
  assign(paste0('p',month[i]), p0)
  assign(paste0('e',month[i]), e0)
if(i == 1){
  pp = get(paste0('p',month[i]))
  ee = get(paste0('e',month[i]))
}else{
  pp = c(pp,get(paste0('p',month[i])))
  ee = c(ee,get(paste0('e',month[i])))
}
}

for(i in 1:12){
  p0 = get(paste0('p',month[i]))
  e0 = get(paste0('e',month[i]))
  a0 = min(e0,p0)
  assign(paste0('a',month[i]), a0)
}

a.sum = sum(a01,a02,a03,a04,a05,a06,a07,a08,a09,a01,a10,a11,a12)
e.sum = sum(e01,e02,e03,e04,e05,e06,e07,e08,e09,e01,e10,e11,e12)
p.sum = sum(p01,p02,p03,p04,p05,p06,p07,p08,p09,p01,p10,p11,p12)

plot(a.sum)

ae.ratio <- a.sum/(e.sum+0.0001)
plot(ae.ratio)

pe.ratio <- p.sum/(e.sum+0.0001)
pe.ratio <- ifel(pe.ratio >1,1,pe.ratio)
plot(pe.ratio)

aepe.ratio <- pe.ratio-ae.ratio
plot(aepe.ratio)

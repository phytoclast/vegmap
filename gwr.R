

#######################
library(GWmodel)
ex <- c(-160, -110, 10, 40)
br2 <- crop(br1, y=ext(ex))
br10 <- br2|> aggregate(10)
newpts <- br10$elev |> as.points()
plot(br2$elev)
#ex <-  c(-130, -60, 10, 50)
train <- tdata |> subset(x >= ex[1] & x <= ex[2] & y >= ex[3] & y <= ex[4]) #|> st_drop_geometry() |> as.data.frame()
train$z <- train$t07
g <- gwr.basic(z ~ lat+coslon+sinlon+
            relev+
            elev+
            w500+w50+w5000+
            (wind000)+(wind090)+(wind180)+(wind270)+
            (wind045)+(wind135)+(wind225)+(wind315), data = train, regression.points = geom(newpts)[,c("x", "y")], 
            adaptive=F, bw=15,kernel = "gaussian", longlat=F)

# g <- gwr(z ~ lat+coslon+sinlon+
#            relev+
#            elev+
#            w500+w50+w5000+
#            #windmax+windmin+windmean#+
#            (wind000)+(wind090)+(wind180)+(wind270)+
#            (wind045)+(wind135)+(wind225)+(wind315)
#          , 
#          data=as.data.frame(train), 
#          coords=as.matrix(train[,c("x", "y")]), bandwidth=15,
#          fit.points=as.matrix(geom(newpts)[,c("x", "y")]))



inames = intersect(names(g$SDF),names(br10))


br.term <- br10[[inames]] 
br.term[[inames]] <- g$SDF[inames]
resid <- br.term[[1]]
resid[] <- g$SDF$Intercept
br.term <- br.term |> project(br2)
resid <- resid |> project(br2)
br.cov <- br2[[inames]]
plot(resid)
z <- sum(br.cov*br.term)+resid

plot(z)

g <- glm(z ~ lat+coslon+sinlon+
           relev+
           elev+
           w500+w50+w5000+
           # windmax+windmin+windmean#+
         (wind000)+(wind090)+(wind180)+(wind270)+
         (wind045)+(wind135)+(wind225)+(wind315)
         , 
         data=as.data.frame(train), weights = wts0
)
summary(g)
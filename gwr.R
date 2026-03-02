

#######################
library(GWmodel)
ex <- c(-180, -150, 10, 40)
br2 <- crop(br1, y=ext(ex))
br10 <- br2|> aggregate(10)
newpts <- br10$elev |> as.points()
plot(br2$elev)
#ex <-  c(-130, -60, 10, 50)
train <- tdata |> subset(x >= ex[1] & x <= ex[2] & y >= ex[3] & y <= ex[4]) #|> st_drop_geometry() |> as.data.frame()
train$z <- train$t07

train <- trdata. |> subset(x >= ex[1] & x <= ex[2] & y >= ex[3] & y <= ex[4]) #|> st_drop_geometry() |> as.data.frame()
train$z <- train$tr07

train <- pdata. |> subset(x >= ex[1] & x <= ex[2] & y >= ex[3] & y <= ex[4]) #|> st_drop_geometry() |> as.data.frame()
train$z <- train$p05

g <- gwr.basic(z ~ lat+coslon+sinlon+
                 relev+
                 elev+
                 w500+w50+w5000
                , data = train, regression.points = geom(newpts)[,c("x", "y")], 
               adaptive=F, bw=5,kernel = "gaussian", longlat=F)

g <- gwr.basic(z ~ lat+coslon+sinlon+
                 relev+
                 elev+
                 w500+w5000+
                 (wind000)+(wind090)+(wind180)+(wind270)+
                 (wind045)+(wind135)+(wind225)+(wind315), data = train, regression.points = geom(newpts)[,c("x", "y")], 
               adaptive=F, bw=15,kernel = "exponential", longlat=F)

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


br.term <- rasterize(vect(g$SDF), y=br10, field=inames)
resid <- rasterize(vect(g$SDF), y=br10, field='Intercept')
br.term <- br.term |> project(br2)
resid <- resid |> project(br2)
br.cov <- br2[[inames]]
plot(resid)
z <- sum(br.cov*br.term)+resid

# plot(z)
# points(train, cex=0.1)

z1 <- z |> crop(ext(-162, -152, 17, 25))
plot(z1)
points(train, cex=0.1)

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
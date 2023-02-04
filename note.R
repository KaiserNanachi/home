library(tidyverse)
library(summarytools)
library(mapview)
summary(map5)

map6<-map5 %>% 
  mutate(hotspot=if_else(exp2>=5.8,1,0)) %>% 
  filter(hotspot==1)
sfa<-intersec %>% 
  select(geometry,total) %>% 
  mutate(hot=if_else(total>=27,1,0)) %>% 
  filter(hot==1)

mp<-mapview::mapview(sfa,
                     zcol="hot",
                     alpha = 0,
                     col.regions = c("orange"),
                     map.types =c("Esri.WorldImagery","OpenStreetMap"))

mapview::mapview(x=map6,
                 zcol="hotspot",
                 col.regions = "red",
                 color="transparent")+
  (mp)


map6$exp2
map7<-map6 %>% 
  filter(hotspot==1)

sfa1<-sfa %>% 
  filter(hot==1)

view(dfSummary(map6))

sfa<-intersec %>% 
  select(geometry,total) %>% 
  mutate(hot=if_else(total>=22,1,0)) %>% 
  filter(hot==1)

m<-mapview::mapview(sfa,
        zcol="hot",
        alpha = 0,
        col.regions = c("grey"),
        map.types =c("Esri.WorldImagery","OpenStreetMap"))


norm<-sfa$total



summary(map6$exp2)

library(sp)

geo<-map5 %>% 
  select(geometry)

loglikelihood <- function(x, y){
  sum(log(dnbinom(y, mu = x[1], size = x[2])))
}

optim(c(10, 20), loglikelihood, y = map6$exp2, control = list(fnscale = -1))


data(meuse)
coordinates(intersec) = ~x+y

data(meuse.grid)
gridded(meuse.grid) = ~x+y

m <- vgm(.59, "Sph", 874, .04)
# ordinary kriging:
x <- krige(log(zinc)~1, meuse, meuse.grid, model = m)

spplot(x["var1.pred"], main = "ordinary kriging predictions")
spplot(x["var1.var"],  main = "ordinary kriging variance")
# simple kriging:
x <- krige(log(zinc)~1, meuse, meuse.grid, model = m, beta = 5.9)
# residual variogram:
m <- vgm(.4, "Sph", 954, .06)
# universal block kriging:
x <- krige(log(zinc)~x+y, meuse, meuse.grid, model = m, block = c(40,40))
spplot(x["var1.pred"], main = "universal kriging predictions")

# krige0, using user-defined covariance function and multiple responses in y:
# exponential variogram with range 500, defined as covariance function:
v = function(x, y = x) { exp(-spDists(coordinates(x),coordinates(y))/500) }
# krige two variables in a single pass (using 1 covariance model):
y = cbind(meuse$zinc,meuse$copper,meuse$lead,meuse$cadmium)
x <- krige0(zinc~1, meuse, meuse.grid, v, y = y)
meuse.grid$zinc = x[,1]
spplot(meuse.grid["zinc"], main = "zinc")
meuse.grid$copper = x[,2]
spplot(meuse.grid["copper"], main = "copper")

# the following has NOTHING to do with kriging, but --
# return the median of the nearest 11 observations:
x = krige(zinc~1, meuse, meuse.grid, set = list(method = "med"), nmax = 11)
# get 25%- and 75%-percentiles of nearest 11 obs, as prediction and variance:
x = krige(zinc~1, meuse, meuse.grid, nmax = 11, 
          set = list(method = "med", quantile = 0.25))
# get diversity (# of different values) and mode from 11 nearest observations:
x = krige(zinc~1, meuse, meuse.grid, nmax = 11, set = list(method = "div"))


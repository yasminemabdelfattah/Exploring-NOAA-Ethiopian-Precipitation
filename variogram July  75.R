###################################################################
##### YEAR 1975 July
###################################################################
###################################################################

library(ggplot2)
library(ggmap)
library(maps)
library(mapproj)
library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)
library(dismo)

###################################################################
##1##Data
###################################################################

# 1- Data -----------------------------------------------------------------
setwd("D:/PhD Ya Rab ya karim efrigha men3andak/PhD Data Analysis/My PhD R work")

load("D:/PhD Ya Rab ya karim efrigha men3andak/PhD Data Analysis/My PhD R work/R data yasmine/yr757.RData")
### Data for spatial point data frame
yr757.c <- yr757[complete.cases(yr757), ]
yr757.cc <- yr757[complete.cases(yr757), ]

fix(yr757.c)
coordinates(yr757.c)=~LON+LAT
proj4string(yr757.c) <- "+proj=utm +zone=37 +datum=WGS84 +units=m"
class(yr757.c)
summary(yr757.c)
# mean rain  214.9
sd(yr757.c$rain)
# SD 134.4708
str(yr757.c)
# access various slots of the SPDF
bbox(yr757.c)


### DATA FRAME
yr757.eda <- yr757[complete.cases(yr757), ]

#### Ploting Rain Vs. Alt
plot(yr757.eda$rain~yr757.eda$ALT)


### Define data as geodata
yr757.g <- yr757[complete.cases(yr757), ]
yr757.g<-yr757.g[c("LAT", "LON","ALT", "station", "year", "month","rain", "COUNTRY", "NOAA_ID")]
library(geoR)
yr757.g<- as.geodata(yr757.g, coords.col = 1:2, data.col =7 )
dup.coords(yr757.g)
class(yr757.g)
### Summary statistics
summary(yr757.eda)
sd(yr757.eda$rain)
### Ploting Geodata
plot(yr757.g)
shapiro.test(yr757.c$rain)



###################################################################
##2## Area Shapefile 
###################################################################

# 2- Area Shapefile  ---------------------------------------------------


# load up area shape file:
library(maptools)
### Read spatial shapefile (Ethiopian adm0 from Global Admintrative area)
area <- readShapePoly(file.choose())
area<- readShapeSpatial(file.choose())

##function summary provides us with some useful information
summary(area)
##manually define the coordinate system by setting the coordinate system information 
proj4string(area) <- "+proj=utm +zone=37 +datum=WGS84 +units=m"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
names(area)
data.frame(area)
###a choropleth map (choro = area and pleth = value) where some value (an enumeration of population in this working example) is aggregated over a defined area (e.g.? administrative units) and displayed using different colors.
spplot(area, z="Shape_Leng", cuts= 4)


#Next we can set the colors we want to use. And then we can set up our basemap.
library(RColorBrewer)
colors <- brewer.pal(9, "Blues")
colors <- brewer.pal(9, "YlGnBu")

## Upload Google Map
library(ggmap)
mapImage <- get_map(location = c(33.8, 5, 44.6, 15.6), maptype = "terrain", zoom = 5)
plot(mapImage)

## Next, we can use the fortify function from the ggplot2 package. 
#This converts the crazy shape file with all its nested attributes 
#into a data frame that ggmap will know what to do with.
area.points <- fortify(area)

## Finally, we can map our shape files!
## 
ggplot(data = sfn , aes(x = long, y = lat, group = group)) + geom_path()
class(sfn)
names(sfn )
sfn=fortify(sfn)

SFNeighbourhoods  = merge(sfn.f, sfn@data, by.x = 'id', by.y = 'ZONENAME')
sf = merge(SFNeighbourhoods, yr757.c, by=c("long", "lat"))


gg <- gg +geom_point(data=adm1.centroids.df, aes(x=long, y=latitude, colour = rain))

ggmap(mapImage) +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = area.points, color = colors[9],
               fill = colors[6], alpha = 0.5) +
  labs(x = "Longitude",y = "Latitude")

###################################################################
##3## Spatial Grid  DataFrame
###################################################################

# # 3- Spatial Grid  DataFrame --------------------------------------------


#To get a grid based on a shapefile you can use the command "spsample".
# cellsize is in map units (e.g. km), also see "?spsample"
plot(area)
points(spsample(area, n = 2000, "regular"), pch = 3)
points(spsample(area, n = 1000, "random"), pch = 3)
grd=spsample(area.points, n = 2000, "regular")

gridded(grd) = TRUE # Make it a grid
class(grd)
# Visualize the grid
image(grd)
summary(grd)
crs(grd) <- "+proj=utm +zone=37 +datum=WGS84 +units=m"
###################################################################
##4## Mapping
###################################################################

# #4- Mapping -------------------------------------------------------------


# library rworldmap provides different types of global maps, e.g:
library(rworldmap)
data(countriesLow)
head(countriesLow@data)
#### Choose Border line for Ethiopia
ethmap=countriesLow[countriesLow$NAME_SORT == "Ethiopia",]
head(ethmap)
plot(ethmap)
class(ethmap)
str(ethmap)
# Have a look
plot(yr757.c, pch=4, col='blue', cex=yr757.c$rain/1000, add=TRUE)
plot(ethmap, add=TRUE)


###################################################################
###################################################################

## we can visually inspect how rain varies over the domain of interest where we map magnitude to point size:
library(ggplot2)
+ggplot(aes(x1, x2), data=yr757.c) + geom_point(aes(size=rain), color="blue", alpha=3/4) + 
  ggtitle("Rain magnitude p(mm)") + coord_equal() + theme_bw()

###################################################################
###################################################################

library(ggplot2)
library(ggmap)
library(maps)
library(mapproj)
library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)
library(dismo)

mymap <- gmap("Ethiopia")  # choose whatever country
plot(mymap)

mymap <- gmap("Ethiopia", type = "satellite")
plot(mymap)

coordinates(yr757.cc) = ~LON+LAT

gbmap.merc <- Mercator(yr757.cc)  
# Google Maps are in Mercator projection. 
# This function projects the points to that projection 
# to enable mapping
points(gbmap.merc, pch = 20, col = "red")


map<- get_map(location = c(33.8, 5, 44.6, 15.6), zoom = 6)
ggmap(map)

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

elbow_room1 <- ggmap(map) + 
  geom_point(data = yr757, aes(x=LON, y=LAT,fill = rain), color = "blue") +
  geom_point(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

p=ggmap(map)+geom_point(data=yr757,alpha = .7, aes(x=LON, y=LAT),color='blue')
p=p+ geom_jitter( data=yr757, position=position_jitter(width=0.6, height=0.6), aes(x=LON, y=LAT, size = rain,color=station))+ scale_size(name="Rain Magnitude") 
p

###########################################################
## GRid VS. spatial points
###########################################################

# load spatial domain to interpolate over
# to compare, recall the bubble plot above; those points were what there were values for. this is much more sparse
plot1= ggplot(aes(LON, LAT), data=yr757.eda) + geom_point(size=1) + ggtitle("Points with measurements") + coord_equal()  


# this is clearly gridded over the region of interest
plot2 <- grd %>% as.data.frame %>%
  ggplot(aes(x1, x2)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points at which to estimate") +
  labs(x = "LON",y = "LAT")

library(gridExtra)
grid.arrange(plot1, plot2, ncol = 2)


###################################################################
###5## Inverse distance weighted interpolation
###################################################################

# #5- Inverse distance weighted interpolation -----------------------------


library(gstat) 
proj4string(yr757.c) <- "+proj=utm +zone=37 +datum=WGS84 +units=m"
rain.idw = idw(rain~1, yr757.c, grd)
class(rain.idw)
spplot(rain.idw["var1.pred"], main = "Rainfall inverse distance weighted interpolations")




###################################################################
##6## VARIOGRAM 
###################################################################

# #6- VARIOGRAM  ----------------------------------------------------------


## Empirical variogram as a scatter plot "variogram cloud"
vario=variog(yr757.g, op="cloud") 
plot (vario)

#uvec :a vector with values used to define the variogram binning
vario.c=variog(yr757.g, uvec = seq(from=0, to=12, by=0.5)) 
plot(vario.c)
## Robust empirical variogram 
vario.r=variog(yr757.g, uvec= seq(from=0, to=12, by=.5), estimator.type="modulus")
plot(vario.r)
## Robust estimate is larger than MOM
plot(vario.c$v, vario.r$v, xlab="MOM estimator", ylab="modulus")
abline(a=0, b=1) 
## MOM greatly affected by a small number of outlying values. Robust estimator is an indiator of what is going on in the bulk of the distribution

## Directional variogram
vario4=variog4(yr757.g)
plot(vario4, same=FALSE) 

###################################################################
### OLS Theoritical Variogram
###################################################################
vario.c=variog(yr757.g, uvec = seq(from=0, to=12, by=0.5)) 
plot(vario.c)
ols=variofit(vario.c, ini.cov.pars= c(150,5),kappa=10,fix.nugget=TRUE, wei="equal" )
summary(ols)
lines(ols,col=4)
ols2<-variofit(vario.c, ini=c(150,5),fix.nugget=TRUE ,kappa=1.5, cov.model="exponential", wei="equal" )
lines(ols2,col=2)
ols4<-variofit(vario.c, ini=c(150,5),fix.nugget=TRUE , cov.model="spherical", wei="equal" )
lines(ols4,col=3)
legend(3, 200, inset=.05, cex=.5,bty='n',
       c("Matern", "Exponential", "Spherical"), 
       col=c(2:5), lty=c(1,4), merge = TRUE )
## WLS
wls=variofit(vario.c, ini.cov.pars = c(150,5), fix.nugget = TRUE,kappa=10)
summary(wls)
plot(vario.c)
lines(wls, lwd=2)
lines(ols, lty=2, lwd=2)
legend(3, 200, inset=.05, cex=.6,bty='n',
       c("WLS", "OLS"), 
       col=c("black", "black"), lty=c(1,4), merge = TRUE )

## MLE
ml0.5 <- likfit(yr757.g, ini=c(150,5), kappa=0.5, fix.nug = TRUE)
ml0.5

ml1.5 <- likfit(yr757.g, ini=c(150,5), kappa=1.5, fix.nug = TRUE)
ml1.5

reml0.5 <- likfit(yr757.g, ini=c(150,5),
                  kappa=0.5, fix.nug = TRUE, met = "REML")
reml0.5

reml1.5 <- likfit(yr757.g, ini=c(150,5),
                  kappa=1.5, fix.nug = TRUE, met = "REML")
reml1.5

ml0.5$AIC
ml1.5$AIC
reml0.5$AIC
reml1.5$AIC
ml0.5$BIC
ml1.5$BIC
reml0.5$BIC
reml1.5$BIC
plot(vario.c)
lines(wls, lty=1, lwd=2, col="black")
lines(ols, lty=2,lwd=2, col="red")
lines(ml0.5, lty=3, lwd=2, col="yellow")
lines(ml1.5, lty=4, lwd=2, col="blue")
lines(reml0.5,lty=1, lwd=2, col="gray")
lines(reml1.5,lty=2,lwd=2,col="green")
legend(2.5, 200, inset=.05, cex=.5,bty='n',c("WLS", "OLS", "ML0.5","ML1.5", "REML0.5", "REML1.5"),
       col = c("black","red", "yellow","blue", "gray", "green"), 
       lty = c(1,2, 3, 4, 1, 2), lwd=c(2, 2, 2, 2, 2, 2), merge = TRUE)


###################################################################
##7## Visualization of Variogram
###################################################################

#7- Visualization of Variogram ------------------------------------------


# For working with spatial (and spatio-temporal) data, we use the gstat package, which includes functionality for kriging, among other many things.
library(sp)
library(gstat)
# packages for manipulation & visualization
suppressPackageStartupMessages({
  library(dplyr) # for "glimpse"
  library(ggplot2)
  library(scales) # for "comma"
  library(magrittr)
})



##a variogram could be fit as simply as the following code:
vgm <- variogram(rain~1, yr757.c) # calculates sample variogram values 
plot(vgm)
vgm.fitg757 <- fit.variogram(vgm, model=vgm(14000, "Gau", 3, 6000))# fit model
plot(vgm, vgm.fitg757)
vgm.fitm757 <- fit.variogram(vgm, model=vgm(14000, "Mat", 3, 6000)) # fit model
plot(vgm, vgm.fitm757)

# Arrange the data for the ggplot2 plot
# add the semivariance values of v2 to v1
Fitted <- data.frame(dist = seq(0.01, max(vgm$dist), length = 101))
Fitted$Gaussian <- variogramLine(vgm.fitg757, dist_vector = Fitted$dist)$gamma
Fitted$Matern <- variogramLine(vgm.fitm757, dist_vector = Fitted$dist)$gamma
#convert the dataframes to a long format
library(reshape2)
Empirical <- melt(vgm, id.vars = "dist", measure.vars = "gamma")
Modeled <- melt(Fitted, id.vars = "dist", measure.vars = c("Gaussian", "Matern"))
library(ggplot2)
#both variogram on the same plot with different colours
pdf("C:\\Users\\Yasmine\\Dropbox\\PhD Data Analysis\\vgmfit757.pdf")
v757=ggplot(Modeled, aes(x = dist, y = value, colour = variable )) + 
  geom_line() +  
  xlab("Distance") +
  ylab("Semivariance") +   ggtitle("July")+ 
  geom_point(data = Empirical)
dev.off()

###################################################################
##7##  Kriging
###################################################################

#8-  Kriging ----------------------------------------------------------


#The following code produces prediction and standard error estimates at the fine grid mesh locations. Note that all observations are used in each model to predict each location on the grid mesh.
#########################################
#Simple Kriging to the prediction grid, mean = 214.9
#########################################
rain.sk <- krige(rain ~ 1, yr757.c, grd, model = vgm.fitg757, beta=214.9)
pdf("C:\\Users\\Yasmine\\Dropbox\\PhD Data Analysis\\sk757.pdf")
##These results could be visualized as a heatmap:
rain.sk %>% as.data.frame %>%
  ggplot(aes(x=x1, y=x2)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "yellow", high="blue") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw() +  
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("July")
dev.off()

#########################################
#Ordinary Kriging to the grid
#########################################
rain.oK <- krige(rain ~ 1, yr757.c, grd, model = vgm.fitg757)
pdf("C:\\Users\\Yasmine\\Dropbox\\PhD Data Analysis\\ok757.pdf")
k757=rain.oK %>% as.data.frame %>%
  ggplot(aes(x=x1, y=x2)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "yellow", high="blue") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw() +  
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("July")
dev.off() 

#########################################
#Universal (with Trend) to the grid, using the trend model form identified above
#########################################
rain.uK <- krige(rain ~I(x1*x2), yr757.c, grd, model = vgm.fitg757)
pdf("C:\\Users\\Yasmine\\Dropbox\\PhD Data Analysis\\uk751.pdf")
rain.uK %>% as.data.frame %>%
  ggplot(aes(x=x1, y=x2)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "yellow", high="blue") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw() +  
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("July")
dev.off()
#########################################
########################################
library(Rmisc)
multiplot(v751, k751, cols=2)
#########################################
#########################################

#Results are summarized and mapped with this code:
pred.sk <- data.frame(predictions=rain.sk$var1.pred, kvar=rain.sk$var1.var, type=rep('sk',length(rain.sk$var1.pred)))
pred.ok <- data.frame(predictions=rain.oK$var1.pred, kvar=rain.oK$var1.var, type=rep('ok',length(rain.oK$var1.pred)))
pred.uk <- data.frame(predictions=rain.uK$var1.pred, kvar=rain.uK$var1.var, type=rep('uk',length(rain.uK$var1.pred)))
preds.krig <- data.frame(rbind(pred.sk,pred.ok,pred.uk))
dev.off()
boxplot(predictions ~ type, preds.krig, col=c('steelblue', 'salmon', 'thistle4'), main='Kriging Prediction Distributions')
boxplot(kvar ~ type, preds.krig, col=c('steelblue', 'salmon', 'thistle4'), main='Kriging Variance Distributions')
spplot(rain.uK, zcol='var1.pred', main="Rainfall Estimates (SK)", sp.layout=pts)
spplot(rain.uK, zcol='var1.var', main="Rainfall SK Variance", sp.layout=pts)
spplot(rain.uK, zcol='var1.var', main="Rainfall SK Standard Errors", formula=sqrt(var1.var)~x1+x2, sp.layout=pts) 
##From this we see that the rainfall magnitude tend to be higher closer to the north-western part of Ethiopia (var1.pred is the predicted measurement for the variable being interpolated). Moreover, lzn.kriged contains the variance of each 

###################################################################
##8## Trend Surface Analysis
###################################################################


#8'-  Trend Surface Analysis ---------------------------------------------


# While no first order spatial trend is visually evident,
#trend surface analysis via OLS was attempted anyway. A full quadratic was employed, and reduced in stepwise fashion. Code for the final model is:
fix(yr757.eda)
rain.tsm <- lm(rain~ I(x1*x2), data=yr757.eda)
summary(rain.tsm)
library(sp)
grd$tsm.pred <- predict(rain.tsm, grd)
pts <- list('sp.points', yr757.eda, pch=1, cex=0.7, col='black')
spplot(grd, zcol='tsm.pred', first= FALSE, scales=list(draw=T), main="Rainfall Estimates (Trend Surface Model)", sp.layout=pts, col=NA)

vgram <- variogram(resid(rain.tsm) ~ 1, ~x1 + x2, yr757.eda)
plot(vgram)
vmod<- vgm(psill=260,model="Gau",range=3.7, nugget=130)
plot(vgram, model=vmod, main="Rainfall TSM Residuals")

#################################################################
#################################################################
##9## Geostatistics, Rainfall and R
#################################################################
###################################################################


#9- Geostatistics, Rainfall and R ---------------------------------------


library(sp)
library(rgdal)
library(gstat)

# Create a variogram
# Display some possible models
plot(gstat::variogram(rain ~ 1 , yr757.c))
# Once we have a visually good variogram, use those parameters:
vg=gstat::variogram(rain ~ 1 , yr757.c)

vg.fit=fit.variogram(vg, vgm(14000, "Gau", 3, 6000))
fit.variogram(vg, vgm(14000, "Gau", 3, 6000))

# Here are the fitted variogram parameters:
print(vg.fit)
plot(vg, vg.fit)


# perform ordinary kriging prediction:
# make gstat object to hold the krige result:
g= gstat(formula=rain ~ 1, data=yr757.c, model=vg.fit)
precip.krige= predict(g, model=vg.fit, newdata=grd)


#Cross validationOK:
cv_ok<- gstat.cv(g,nfold=5) 
 
#10- Contouring ---------------------------------------------------------

# Some parameters for plotting
par(font.main=2, cex.main=1.5,cex.lab=0.4, cex.sub=1)
# Use the ColorBrewer library for color ramps
library(RColorBrewer)
precip.pal = colorRampPalette(brewer.pal(7, name="Blues"))
# plot the krige interpolation
spplot(precip.krige,  zcol='var1.pred',col.regions=precip.pal, contour=TRUE, col='black', pretty=TRUE,main="Interpolated Rainfall - January", sub="Ordinary Kriging", labels=TRUE)
       

# Now check for correlation between precipitation and elevation
cor.test(yr757.c$ALT, yr757.c$rain)

# Create a variogram for ALT
# Display some possible models
plot(gstat::variogram(ALT ~ 1 , yr757.c))
# Once we have a visually good variogram, use those parameters:
vgalt=gstat::variogram(ALT ~ 1 , yr757.c)

vg.fitalt=fit.variogram(vgalt, vgm( 150000, "Gau", 3, 100000))
fit.variogram(vgalt, vgm( 150000, "Gau", 3, 100000))

# Here are the fitted variogram parameters:
plot(vgalt, vg.fitalt)

library(gstat)
# recreate g, the gstat object with a second variable
rm(g)
g = gstat(id="rain", formula=rain ~ 1, data=yr757.c, vgm(14000, "Gau", 3, 6000))
g = gstat(g, id="ALT", formula=ALT ~ 1, data=yr757.c, model=vgm( 150000, "Gau", 3, 100000))
vg = gstat::variogram(g)
# Make the multivariable variogram (Linear Model of Coregionalization)
vm.fit = fit.lmc(vg, g, vgm(100, "Gau",2, 1000))
# Graph the model and experimental variograms
plot(vg, vm.fit)
#Perform cross-validation:       
cv_ck <- gstat.cv(vm.fit)


# Now predict() should create a CoKriging interpolation
precipcokrige=predict(vm.fit, newdata=grd)
class(precipcokrige)



#### Countor map with values : Cokriging
pdf("D:\\PhD Ya Rab ya karim efrigha men3andak\\PhD Data Analysis\\My PhD R work\\PDF output\\ssplot757.pdf")
spplot(precipcokrige,  zcol='var1.pred',col.regions=precip.pal, contour=TRUE, 
  col='black', pretty=TRUE,main="July", 
  labels = list(cex = 0.7),
                     label.style = 'align',
                     margin = TRUE,   width = 2, cex = 2)
dev.off()


precipcokrige=as.data.frame(precipcokrige)
library(ggplot2)
library(directlabels)
#### ggplot2 contour map without values
pdf("D:\\PhD Ya Rab ya karim efrigha men3andak\\PhD Data Analysis\\My PhD R work\\PDF output\\vgmfit757.pdf")
k757<- ggplot(precipcokrige, aes(x1, x2, z = var1.pred), colour = ..level..)
k757=k757 + geom_raster(aes(fill = var1.pred)) +
  geom_contour(colour = "white")+ggtitle("July") +
  labs(x = "Longitude",y = "Latitude")
k757+guides(fill = guide_legend("rain.pred"))
k757
dev.off()


#### google map with contours without values
pdf("D:\\PhD Ya Rab ya karim efrigha men3andak\\PhD Data Analysis\\My PhD R work\\PDF output\\ggplot757.pdf")
p=ggmap(map) 
p+stat_contour(aes(x1,x2,z=var1.pred, colour =..level..), data=precipcokrige)
+ geom_raster(aes(fill = var1.pred))
dev.off()



##############
#COMPARE ORDINARY KRIGING WITH CO-KRIGING:
sum(cv_ok$residual^2)

sum(cv_ck$residual^2)

summary(cv_ok)
summary(cv_ck)

# mean error, ideally 0:
mean(cv_ok$residual)
mean(cv_ck$residual)

# absolute mean error AME
mean(abs(cv_ok$residual))
mean(abs(cv_ck$residual))

# Mean squared prediction error MSPE, ideally small
mean(cv_ok$residual^2)
mean(cv_ck$residual^2)

#RMSE (root mean squared error)
sqrt(mean(cv_ok$residual^2))
sqrt(mean(cv_ck$residual^2))


# correlation observed and predicted, ideally 1
cor(cv_ok$observed, cv_ok$observed - cv_ok$residual)
cor(cv_ck$observed, cv_ck$observed - cv_ck$residual)

# correlation predicted and residual, ideally 0
cor(cv_ok$observed - cv_ok$residual, cv_ok$residual)
cor(cv_ck$observed - cv_ck$residual, cv_ck$residual)


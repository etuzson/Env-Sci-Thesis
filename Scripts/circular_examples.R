# movement analysis I - circular statistics

x<-c(1:10)
plot(x,sin(x),type="l")

curve(sin(x),xlim=c(0,2*pi))
curve(x*1,add=TRUE)


require(circular)             # for doing circular statistics
?circular                     # help for function circular in package circular


# play around with circular data

deg(1)                  # degrees vs radians
rad(57.29578)

?rad                    # find out what rad does

data(ncfrog) 
str(ncfrog)             # list the structure of the dataset ncfrog
ncfrog                  # vector
rad(ncfrog)             # convert to radians
ncfrog.rad              # vector in radians
deg(ncfrog.rad)         # convert to degrees

rose.diag(ncfrog, col=8, prop=1.5, ticks=FALSE)
rose.diag(ncfrog.rad, col=4, prop=1.5, ticks=TRUE)

# descriptive statistics

?pigeons

head(pigeons)        # pigeons is a data frame in the circular library
str(pigeons)
str(pigeons$bearing) # not yet in circular format
pigeons$bearing<-circular(pigeons$bearing,units=c("degrees"),
template="geographics",modulo="asis",rotation="clock",type="angles",
zero=1.57)           # converts to circular format; note the many options
plot(pigeons$bearing)  # zero = 1.57 means 0 degrees = N

?circular


str(pigeons$bearing) # now in circular format
pigeons$rad <- rad(pigeons$bearing)  # you can always switch between radians and degrees
str(pigeons$rad)     # note the formatting has not changed
head(pigeons)
plot(pigeons$bearing)
pigeons$rad <- circular(pigeons$rad,units=c("rad"))
str(pigeons$rad)
plot(pigeons$rad)
plot(pigeons$bearing)


mean(pigeons$bearing, na.rm=FALSE)                            # mean angle (sqrt of x^2 + y^2)
median.circular(pigeons$bearing, na.rm = FALSE)               # median angle
range.circular(pigeons$bearing)                               # range
summary(pigeons$bearing) # mean and resultant distance, rho
rho<-rho.circular(pigeons$bearing)
rho
var(pigeons$bearing)     # 1-rho, a meausre of dispersion

angular.deviation(pigeons$bearing)   # angular deviation in radians
angular.variance(pigeons$bearing)    # angular variance = angular deviation squared


# find simple bootstrap confidence intervals for the parameters of a von Mises distribution
# confidence on mean direction mu and dispersion kappa

CI<-mle.vonmises.bootstrap.ci(pigeons$bearing, mu = NULL, bias = FALSE, alpha = 0.05,
reps = 1000)
str(CI)
CI$mu.ci

set.seed(1234)  # initializes the pseudorandom number algorithm & allows you to replicate simulation results between runs
x1 <- rvonmises(n=100, mu=circular(pi), kappa=2) # creates a distribution of data around pi in radians, kappa controls the variance
x2 <- rcircularuniform(100,control.circular=list(units="degrees"))
x2 <- rcircularuniform(100)
x2
plot(x1)
plot(x2)

# plot angles

plot(pigeons$bearing, pch = 16, cex = 1, stack = FALSE,
axes = TRUE, sep = 0.025, shrink = 1, bins = NULL, ticks = FALSE,
tcl = 0.025, tcl.text = 0.125, col = NULL, tol = 0.04, uin = NULL,
xlim = c(-1, 1), ylim = c(-1, 1), digits = 2, units = NULL,
template = NULL, zero = NULL, rotation = NULL,
main = NULL, sub=NULL, xlab = "huh?", ylab = "",
control.circle=circle.control(col=4,lty=1))

# can also plot a circular object without specifying details
plot(pigeons$bearing)

# add points to an existing plot
points(median.circular(pigeons$bearing),pch=21,bg=8,cex=2)   # add=TRUE won't work
points(mean(pigeons$bearing),pch=21,bg=4,cex=2,col=4)


?points
?lines

# add arrows or lines to existing plot
# length controls the arrow head length - 0 for no arrow head

arrows.circular(CI$mu.ci, y = NULL, x0 = 0, y0 = 0, na.rm = FALSE,
shrink = 1, plot.info = NULL, zero = NULL, rotation = NULL,length=0.2,col=4,lwd=1)

arrows.circular(pigeons$bearing, length=0)
arrows.circular(mean(pigeons$bearing),lwd=4,length=0)
arrows.circular(mean(pigeons$bearing),rho.circular(pigeons$bearing), lwd=2,length=0.1)

# plot rose diagram {circular}

plot.circular(pigeons$bearing)

rose.diag(pigeons$bearing, prop = 2, bins=25,shrink=0.9)

rose.diag(pigeons$bearing, pch = 16, cex = 1, axes = TRUE, shrink = 1, 
  bins = 30, upper = TRUE, ticks = TRUE, tcl = 0.025, tcl.text = 0.125,
  radii.scale = c("sqrt", "linear"), border=NULL, col=NULL, tol = 0.04,
  uin = NULL, xlim = c(-1, 1), ylim = c(-1, 1), prop = 2, digits = 2, 
  plot.info = NULL, units = NULL, template = NULL, zero = NULL, 
  rotation = NULL, main = NULL, sub = NULL, xlab = "", ylab = "",
  add = FALSE)

# goodness of fit

# general alternative
rayleigh.test(pigeons$bearing, mu=NULL)   #test of uniformity, mean of resultant length
# Specified alternative
rayleigh.test(pigeons$bearing, mu=circular(0))
plot(circular(pi))

# Watson's goodness of test for the von Mises or circular uniform distribution
watson.test(pigeons$bearing, alpha=0, dist=c("uniform"))
watson.test(pigeons$bearing, alpha=0, dist=c("vonmises"))

# Watson's two-sample test of homogeneity with specified alpha

test1 <- rvonmises(n=20, mu=circular(0), kappa=3)
test2 <- rvonmises(n=20, mu=circular(pi), kappa=2)
plot(test1)
points(test2,col="green")
watson.two.test(test1, test2, alpha=0.05)


# library data

data(fisherB11c) # sea stars
plot(fisherB11c, stack=TRUE, shrink=1.5)
data(fisherB7c) # ants
plot(fisherB7c, zero=pi/2, rotation='clock', stack=TRUE)



# translate location data to movement data of class ltraj

library(adehabitatLT) 

# puechabonsp has trajectories of 4 wild boars
# need to create an object of class ltraj
# to do this, date needs to be of class POSIXct - vector of seconds since January 1st, 1970

data()                  # lists all data sets available
data(puechabonsp)       # loads the puechabonsp data set from the package adehabitatLT
locs <- puechabonsp$relocs    # relocs has spatial coordinates and times
str(locs)
locs <- as.data.frame(locs)
head(locs)
da <- as.character(locs$Date)
head(da)
da <- as.POSIXct(strptime(as.character(locs$Date),"%y%m%d"))
head(da)
puech <- as.ltraj(xy = locs[,c("X","Y")], date = da, id = locs$Name)
puech                   # has four elements corresponding to four ids
burst(puech)            # names of bursts
plot(puech)             # plot 4 trajectories

# note that some datasets may already have data in class ltraj

#########################################
# management of data in class ltraj     #
#########################################

puech
puech[1]                # lists first element (burst) of class ltraj
head(puech[[1]])        # lists x, y, date, dx, dy, dist, dt, etc. for first element
plot(puech[1])          # plots first burst
puech2 <- puech[c(1,2)] # selects first two bursts 
bu <- which.ltraj(puech, "dist>2000")   # select attributes of trajectories
bu$burst
puech[[4]]
plot(puech[burst(puech) %in% bu$burst]) # extract burst data from bu
burst(puech)

##########################
# sometimes you need data in class data.frame, sometimes in class ltraj
##########################

puech2<-ld(puech)       # convert class ltraj to class data.frame
head(puech2)
str(puech2)
puech<-dl(puech2)       # convert class data.frame to ltraj

##########################
# plotting               #
##########################

plot(puech)             # plot trajectories
plotltr(puech, "dist")  # plot sequence of "dist" for each element; note #3
plotltr(puech, "x")
plotltr(puech, "dt")  
plotltr(puech, "dt/3600/24")
plotltr(puech, "rel.angle")

##########################
# Descriptive statistics  #
##########################

# To analyze autocorrelation using the function acfang.ltraj or acfdist.ltraj,
# require a regular path (even intervals)

data(bear)            # has a regular path
plot(bear)
str(bear)             # must be class ltraj, type II, and regular for wawotest.ltraj
wawotest.ltraj(bear)  # tests for random distribution of values in a vector (a is the statistic, p = p-value)
acfdist.ltraj(bear,lag=10)   # serial autocorrelation plot of move distance
acfang.ltraj(bear,lag=10)    # serial autocorrelation plot of rel.angle

# the grey area represents a 95 % interval obtained after permutation of the data.
# If the observed data is outside this region, it is considered significant and 
# represented by a black symbol

bear.df<-ld(bear)       # convert to data frame

mean(bear.df$dist,na.rm=TRUE)      # mean length of move
mean(bear.df$dt,na.rm=TRUE)        # mean time step
mean(bear.df$R2n,na.rm=TRUE)       # squared net displacement (squared distance between location and origin)

head(bear.df)
plot(bear.df$R2n)
require(circular)
bear.df$rel.angle<-circular(bear.df$rel.angle,units=c("radians"),modulo="asis",rotation="clock",type="angles",
zero=1.57)
bear.df$abs.angle<-circular(bear.df$abs.angle,units=c("radians"),modulo="asis",rotation="clock",type="angles",
zero=1.57)

bear.df$rel.angle
bear.df <- na.omit(bear.df)
plot(bear.df$rel.angle)
rose.diag(bear.df$rel.angle,bins = 30, prop = 4)
rose.diag(bear.df$abs.angle,bins = 30, prop = 4)

?abs.angle

# tortuosity, d - from Turchin 1998 
# to estimate, plot apparent path length vs. ruler length from a regular path

head(bear.df)
plot(bear.df$R2n)

n <- 100
fractal.d <- matrix(NA,nrow=n,ncol=2)
fractal.d
for (t in 30:n){
  fractal.d[t,1] <-(sqrt((bear.df$x[t]-bear.df$x[30])^2 + (bear.df$y[t]-bear.df$y[30])^2)) # actual displacement
  fractal.d[t,2] <- t * mean(bear.df$dist) # expected displacement
}

fractal.d
plot(fractal.d[,2],fractal.d[,1],log="xy")   # has problems!
lm(fractal.d[,2]~fractal.d[,1])  # the slope estimates 1-d

# d = 1-slope


# compare actual r2n to expected r2n

# need expected net squared displacement
n <- length(bear.df$dist)
n
n <- 6
m1 <-mean(bear.df$dist,na.rm=TRUE)   # mean move distance
m2 <- mean(bear.df$dist^2,na.rm=TRUE) # mean squared move distance
cos <- as.numeric(mean(cos(bear.df$rel.angle),na.rm=TRUE))
sin <- as.numeric(mean(sin(bear.df$rel.angle),na.rm=TRUE))
alpha <- atan(sin/cos)
temp <- ((1-cos)^2-sin^2)*cos((n+1)*alpha)-2*sin*(1-cos)*sin((n+1)*alpha)
r2n <- n*m2+2*m1^2*(((cos-cos^2-sin^2)*n-cos)/((1-cos)^2+sin^2)+(2*sin^2+(cos+sin^2)^((n+1)/2))/((1-cos)^2+sin^2)^2*temp)
r2n


n <- 800
RCW <- matrix(NA,nrow=n,ncol=1)
RCW
for (t in 1:n){
  #temp <- ((1-cos)^2-sin^2)*cos((t+1)*alpha)-2*sin*(1-cos)*sin((t+1)*alpha)
  #RCW[t,] <- t*m2+2*m1^2*(((cos-cos^2-sin^2)*t-cos)/((1-cos)^2+sin^2)+(2*sin^2+(cos+sin^2)^((t+1)/2))/((1-cos)^2+sin^2)^2*temp)
  RCW[t,] <- t*(m2+2*m1^2*(cos/(1-cos)))
}

plot(RCW)




###########################################
# detecting and dealing with bursts       #
###########################################

plotltr(puech, "dt/3600/24")

# define a function which returns foo when time interval is greater than 100 days 
foo <- function(dt) {
   return(dt> (100*3600*24))
   }

# cut sub-bursts
puech2 <- cutltraj(puech, "foo(dt)", nextr = TRUE)
puech2
plotltr(puech2, "dt/3600/24")

# name the bursts
burst(puech2)[3:4] <- c("Chou.1992", "Chou.1993")  # name burst

puech2

# add in missing values, but don't use this to transform an irregular path into a regular path

# define reference data
refda <- strptime("00:00", "%H:%M")
refda

puech3 <- setNA(puech2, refda, 1, units = "day")
puech3 <- sett0(puech3, refda, 1, units = "day")
puech3

plotltr(puech3, "dt/3600/24")
is.regular(puech3)            # check to see that burst is regular


##############################################
# discretizing to make regular intervals     #
##############################################

data(bear)
plot(bear)
bear
plotltr(bear)

bearI <- typeII2typeI(bear)
str(bearI)

# rediscretize this path with a constant step length of 500 m
bearIr <- redisltraj(bearI, 500)
plot(bearIr)

# cos of angle closer to 0.5 means the trajectory is more tortuous
sliwinltr(bearIr, function(x) mean(cos(x$rel.angle)), type="locs", step=30)
wawotest(bearIr)




#################################
# simulating movement           #
#################################

# simple bivariate Gaussian movement (Royle et al. 2014)

nocc <- 100
Sx <- Sy <- matrix(NA,nrow=1,ncol=nocc)
sigma.move <- 0.2

# simulate initial coordinates on the square
Sx[,1] <- runif(1,0,16)  # random number for col 1
Sy[,1] <- runif(1,0,16)
#Sx[,1]<-10
#Sy[,1]<-10

for (t in 2:nocc){
  Sx[,t] <- rnorm(1,Sx[,t-1],sigma.move)
  Sy[,t] <- rnorm(1,Sy[,t-1],sigma.move)
}


plot(Sx,Sy,type='l',lwd=2)

########################################
#   habitat-dependent movement         #
########################################

# least cost path through habitat matrix

## Make a raster from scratch
library(raster)
r <- raster(nrows=4,ncols=4) 				# grid dimensions
projection(r) <- "+proj=utm +zone=12 +datum=WGS84"	# set projection
extent(r) <- c(0.5,4.5,0.5,4.5)				# extent (min max of xy)
v <- matrix(c(100,   1,   1, 1,
		 100, 100,   1, 1,
		 100, 100, 100, 1,
		 100,   1,   1, 1), nrow=4,ncol=4,byrow=T) # cell specific values
values(r) <- v
plot(r,legend=F)
text(coordinates(r),paste(values(r)),cex=2,font=2)
head(coordinates(r))
v
r

## make a raster from x,y,z data

library(raster)
library(scrbook)
x<-seq(-10,10,0.5)
y<-seq(-10,10,0.5)
xy <- expand.grid(x,y)
z <- e2dist(matrix(c(0,0),1,2),xy)
r <- rasterFromXYZ(cbind(xy,c(z)))
plot(r)
r





# example from 









########################################
## simulate random correlated walk using package adehabitatLT
########################################

mo <- NMs.CRW(N = 1, nlocs = 100, rho = 0, h = 1, x0 = c(0,0),
        treatment.func = NULL,
        treatment.par = NULL, constraint.func = NULL,
        constraint.par = NULL, nrep = 1)

print(mo)
testNM(mo)
# not sure how to plot


# simm.crw

require(adehabitatLT)
require(CircStats)

set.seed(876)
# r is the concentration parameter
# h is scaling parameter for movement length, drawn from chi distribution * h * sqrt(dt)
u <- simm.crw(1:200, r = 0.99, burst = "r = 0.99")
v <- simm.crw(1:200, r = 0.9, burst = "r = 0.9", h = 2)
w <- simm.crw(1:200, r = 0.6, burst = "r = 0.6", h = 5)
x <- simm.crw(1:200, r = 0, burst = "r = 0 (Uncorrelated random walk)",
h = 0.1)
z <- c(u, v, w, x)
plot(z, addpoints = FALSE, perani = FALSE)

?plot
## NMs.randomCRW

require(adehabitatLT)

## first load the data:
data(puechcirc)
data(puechabonsp)
map <- puechabonsp$map

## Consider the first animal
## on an elevation map
anim1 <- puechcirc[1]
plot(anim1, spixdf=map[,1])

## We define a very simple treatment function
## for a NMs model: it just plots the randomized trajectory
## over the study area
## As required, the function takes two arguments:
## x is a data.frame storing a randomized trajectory (three
## columns: the x, y coordinates and the date)
## par contains the map of the study area

myfunc <- function(x, par)
{
   par(mar = c(0,0,0,0))
   ## first plot the map
   image(par)

   ## then add the trajectory
   lines(x[,1], x[,2], lwd=2)
}

## Then we define the null model
##
## We define the range of the study area where the trajectory
## will be shifted:
rxy <- apply(coordinates(map),2,range)
rxy

consfun <- function(x, par)
{
   ## first convert x to the class SpatialPointsDataFrame
   coordinates(x) <- x[,1:2]

   ## then use the function over from the package sp
   ## to check whether all points in x are located inside
   ## the study area
   ov <- over(x, geometry(map))
   return(all(!is.na(ov)))  
}


## We generate correlated random walks with the same starting
## point as the original trajectory, the same turning angle
## distribution, and the same distance between relocation
## distribution. We use the same constraint function as previously
## (all relocations falling within the study area)

mo <- NMs.randomCRW(na.omit(anim1), rangles=TRUE, rdist=TRUE,
                    treatment.func = myfunc,
                    treatment.par = map, constraint.func=consfun,
                    constraint.par = map, nrep=9)

par(mfrow = c(3,3))
tmp <- testNM(mo)


###################################
# analysis of shearwater data     #
###################################

library(rgdal)
require(adehabitatLT)

# problem 1 - date in wrong format
# fix - in Excel, make a new variable datetime formatted as yyyy-mm-dd  hh:mm:ss

test1 <- read.csv("C:/Users/Yolanda/Dropbox/Shearwater data/shearwaters-import.csv")
test1 <- read.csv("C:/Documents and Settings/ymorbey/My Documents/Dropbox/Shearwater data/shearwaters-import.csv")
test1 <- test1[,c(38:45)]
test1 <- na.omit(test1)  

head(test1)
str(test1)  # 176336 observations

table(test1$utm.zone)

# problem - need Cartesian coordinates to estimate move distance in adehabitatLT
# fix - convert UTMs to lat long....

 
list <- by(test1,
            list(zone = test1$utm.zone),
            function(x){
                strg <- paste("+proj=utm +ellps=intl +zone=",
                unique(x$utm.zone), sep = "")
                SP <- SpatialPoints(cbind(x$utm.easting, x$utm.northing),
                proj4string=CRS(strg))
                as.data.frame(spTransform(SP, CRS("+proj=longlat
                +datum=WGS84")))
                })
test2 <- do.call("rbind", list)     # executes a function call
dimnames(test2) <- list(seq(nrow(test2)), c("x", "y"))
head(test2)

list         # is a list of coordinates by utm.zone.... need to merge properly
str(test2)   # has 176336 observations

test3 <- test1[order(test1$utm.zone),]
str(test3)   # has 176336
test3$lat <- test2[,2]
test3$long <- test2[,1]

plot(test3$long,test3$lat)

# sort test3 back to trajectories

test5 <- test3[order(test3$tag.local.identifier,test3$datetime),]
test5$X <- test5$long
test5$Y <- test5$lat
head(test5)
str(test5)  # 176336 obs


# next, use test5 to convert to object of class ltraj

test5$datetime <- as.POSIXct(strptime(as.character(test5$datetime),"%Y-%m-%d %H:%M:%S"))
da <- test5$datetime

# problem - some times not unique within bursts
# fix - delete fixes with dt = 0

test5$datetime1 <- as.numeric(test5$datetime) # make new datetime variable as class numeric
head(test5$datetime1)

test6 <- test5[which(diff(test5$datetime1,1)>0),]
test6 <- test5[which(diff(test5$datetime1,1)>1800),]   # only keep dt > 30 minutes
test6 <- test6[which(test6$tag.local.identifier == "17680"),]  # look at one id
test6$tag.local.identifier <- droplevels(test6$tag.local.identifier)
da <- test6$datetime        # re-define da

str(test6)

shear <- as.ltraj(xy = test6[,c("X","Y")], date = da, id = test6$tag.local.identifier)

shear
plot(shear)
plot(shear[6])
plot(shear[1:4])             # plot 4 trajectories
plot(shear[5:8])


require(circular)

shear.df <- ld(shear[1])

shear.df$rel.angle<-circular(shear.df$rel.angle,units=c("radians"),modulo="asis",rotation="clock",type="angles",
zero=1.57)
shear.df$abs.angle<-circular(shear.df$abs.angle,units=c("radians"),modulo="asis",rotation="clock",type="angles",
zero=1.57)

shear.df <- na.omit(shear.df)
plot(shear.df$rel.angle)
rose.diag(shear.df$rel.angle,bins = 30, prop = 2)
rose.diag(shear.df$abs.angle,bins = 30, prop = 2)

plotltr(shear[1:4], "dist")  # plot sequence of "dist" for each element; note #3
plotltr(shear[1:4], "x")
plotltr(shear[1:4], "dt")  
plotltr(shear[1:4], "dt/3600/24")
plotltr(shear[1:4], "rel.angle")



# problem - coordinates in latlong, so dist not in metres!
# fix - re-calculate movement attributes


shear.df <- ld(shear[1])    # try just on the first burst
head(shear.df)
str(shear.df)     # 3337 observations

# method 1 to create lagged variables (there are many methods)

tempx <- as.matrix(lag(shear.df$x,1))
tempy <- as.matrix(lag(shear.df$y,1))
n <- length(shear.df$x)
shear.df$lagx[1] <- NA   # must define first element as NA
shear.df$lagy[1] <- NA
shear.df$lagx[2:n] <- tempx[1:n-1,]
shear.df$lagy[2:n]<- tempy[1:n-1,]
head(shear.df)    # check

# method 2

shear.df$lagx <- c(NA, embed(shear.df$x,2)[,2])
shear.df$lagy <- c(NA, embed(shear.df$y,2)[,2])
head(shear.df)

# define a function to calculate distances between x,y pairs

gcd.slc <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}

shear.df$dist2 <- gcd.slc(shear.df$x,shear.df$y,shear.df$lagx,shear.df$lagy)
head(shear.df)

plot(shear.df$dist,shear.df$dist2)
head(shear.df[,c("x","y","lagx","lagy","dist","dist2")])
hist(shear.df$dist)
hist(shear.df$dist2/shear.df$dt)
mean(shear.df$dt)


# other movement attributes, including rel.angle, would need to be re-calculated



###############################################
# the package move.... not yet user friendly!
###############################################

require(move)
require(RCurl)

curl <- movebankLogin(username="ymorbey")

getMovebankAnimals(study=80477,login=curl)    # I get an error! 
getMovebankStudy(study=80477,login=curl) 




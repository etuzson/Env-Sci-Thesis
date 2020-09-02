
# Analysis of tailwind and cloud cover at Old Cut
# Extract tailwind and cross wind components on each day of May in 2014 & 2015, April & May 2017, and May 2012
# Run for 10 m, 30 m, 925 mb

Sys.setenv(TZ='GMT')

# It is optional to set your path where libraries are kept.
.libPaths("C:/Users/ymorbey/Documents/R/win-library/3.4") 

install.packages("RNCEP")

require(RNCEP)
require(dplyr)

wx.extent <- NCEP.gather(variable='uwnd', level=925,
months.minmax=c(5), years.minmax=c(2014,2015),
lat.southnorth=c(42.5,42.5), lon.westeast=c(-80,-80),
reanalysis2 = FALSE, return.units = TRUE)

# read in an *.csv file with datetimes
test1 <- read.csv("C:/Users/ymorbey/Documents/Manuscripts - current/Protandry 2014/datetime.csv")
test1$datetime=paste(test1$date, test1$time, sep=" ")
test1$datetime2 = paste(test1$date,"06:00:00",sep=" ")
test1$datetime=as.POSIXct(strptime(test1$datetime,"%m/%d/%Y %H:%M:%S" )) 
test1$datetime2=as.POSIXct(strptime(test1$datetime2,"%m/%d/%Y %H:%M:%S" )) 
test1=subset(test1,select=-c(date,time)) #removes the no longer needed cols
test1

test1$uwind <- NCEP.interp(variable='uwnd', level=925,
lat=42.583, lon=-80.397, dt=test1$datetime,
reanalysis2=TRUE, keep.unpacking.info=TRUE, interpolate.space=T, interpolate.time=T)

test1$vwind <- NCEP.interp(variable='vwnd', level=925,
lat=42.583, lon=-80.397, dt=test1$datetime,
reanalysis2=TRUE, keep.unpacking.info=TRUE,interpolate.space=T, interpolate.time=T)

test1$uwind
test1$vwind

test1$tailwind <- (sqrt(test1$uwind^2 + test1$vwind^2)*cos(((atan2(test1$uwind,test1$vwind)*
(180/pi))-21)*(pi/180)))

# total cloud cover, entire atmosphere, %, 6 h average starting at the reference time
test1$cloud <- NCEP.interp(variable='tcdc.eatm', level='gaussian',
lat=42.583, lon=-80.397, dt=test1$datetime,
reanalysis2=TRUE, keep.unpacking.info=TRUE)

# prate, precipitation rate at surface, in units of Kg/m^2/s
# 6 h averages starting at the reference
test1$prate <- NCEP.interp(variable='prate.sfc', level='gaussian',
lat=42.583, lon=-80.397, dt=test1$datetime,
reanalysis2=TRUE, keep.unpacking.info=TRUE)

test1$temp <- NCEP.interp(variable='air.sig995', level='surface',
lat=42.583, lon=-80.397, dt=test1$datetime,
reanalysis2=TRUE, keep.unpacking.info=TRUE)

test1$pr_wtr.eatm <- NCEP.interp(variable='pr_wtr.eatm', level='surface',
lat=42.583, lon=-80.397, dt=test1$datetime,
reanalysis2=TRUE, keep.unpacking.info=TRUE)


str(test1)
test1$year <- as.POSIXlt(test1$datetime)$year + 1900
test1$yday <- as.POSIXlt(test1$datetime)$yday + 1


tapply(test1$temp,test1$year,mean)-273

mod1 <- lm(temp06 ~ as.factor(year), data=test1)
summary(mod1)

require(lattice)
xyplot(temp06-273~yday,type='b',data=test1,groups=year,
auto.key=list(space='inside'))


glimpse(test1)
View(test1)
test1

# write file to a *.csv
write.csv(test1,file="C:/Users/ymorbey/Documents/Manuscripts - current/Protandry 2014/weather.csv")



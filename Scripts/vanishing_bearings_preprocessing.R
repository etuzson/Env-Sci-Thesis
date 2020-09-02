# 2014 & 2015 Motus data for calculation of vanishing bearings
# Last revised: 15 Nov 2019

Sys.setenv(TZ='GMT')  

# set your working directory
setwd("C:/Users/ymorbey/Documents/Manuscripts - current/Protandry 2014")

require(dplyr)
require(lattice)
library(lubridate)
library(changepoint)
library(ggplot2)
library(maptools)

rm(list=ls())

### import bird data, adjust some departure dates from Old Cut, and select those with mlos at Old Cut >= 2
### This gives me my bird lists for 2014 and 2015.
### Make sure to keep depart_night2, which is the last full day a bird was present at Old Cut.

bird <- read.csv("dryad/morbey_radio.csv",header=T)
bird_data$depart_night2 <- as.POSIXct(strptime(as.character(bird_data$last_day),"%m/%d/%Y"))
attr(bird$depart_night2,"tzone") <- "UTC"

# previous lists of birds used in changepoint analysis:
# filter(id %in% c(417,421,424,425,431,449,466,474,482,487,488,490,491,436,450,457,461,464,465,486,494,438,462,483)) %>%    # birds present at Old Cut for 1 day or gappy
# filter(id %in% c(12,15,20,22,25,27,33,277,289,291,294,295,297,303,304,398,401,405,409,418,420,421)) # gappy birds in 2015

bird.2014 <- bird %>%
   filter(year==2014 & !is.na(mlos == T)) %>%
   select(year,id,mlos,depart_night2) 
( bird.2014.list <- as.factor(bird.2014$id) )  # duplicates in 2015 = 417,418,421. 

bird.2015 <- bird %>%
   filter(year==2015 & !is.na(mlos == T)) %>%
   select(year,id,mlos,depart_night2)
( bird.2015.list <- as.factor(bird.2015$id) )  

( bird2 <- rbind(bird.2014,bird.2015) )
bird2 <- rename(bird2,mfgID=id)

#################################################################################################
### read in motus data as *.rds files 

gc()     # sometimes I run this to clear up memory
ls()

test2014 <- readRDS("protandry2014.rds")
test2014.2 <- test2014 %>%
  filter(mfgID %in% bird.2014.list) %>%
  select(mfgID,recvDeployName,motusTagID,tagProjID,port,ts,sig,sigsd,runLen,freqsd)
rm(test2014)

test2015 <- readRDS("protandry2015.rds")
test2015.2 <- test2015 %>%
  filter(mfgID %in% bird.2015.list & year(ts) == 2015) %>%
  select(mfgID,recvDeployName,motusTagID,tagProjID,port,ts,sig,sigsd,runLen,freqsd)
rm(test2015)

test <- rbind(test2014.2,test2015.2)

rm(test2014.2,test2015.2)

dim(test)

### 

test <- test %>% mutate(year=year(ts))
names(test)
names(bird2)

# merge data & motus data
test <- merge(test,bird2,by.x = c("year","mfgID"), by.y = c("year","mfgID"))
glimpse(test)   


# you can save the data now
saveRDS(test, "data.rds")

#################################################################################################



### Start here once you've created data.rds.

test <- readRDS("test.rds")    

test2 <- test %>% mutate(year = as.integer(year(ts))) %>%
	group_by(year,mfgID) %>% filter(yday(ts) <= yday(depart_night2)) %>%
	filter(runLen > 2 & freqsd < 0.1)


# note that I use mfgID not motusTagID



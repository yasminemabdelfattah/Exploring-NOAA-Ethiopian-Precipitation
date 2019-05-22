
## Empty R Enviromwnt 
rm(list=ls())
## Set working directory
setwd("C:/Users/ALMAGRABY/Google Drive/PhD Thesis/WRRI_data/R script")

###########################################################################
## Manipulating Ethiopia Percipitation Data (origionally it is a wide format)
###########################################################################

## importing data and defining ".." as missing
Data_NOAA_summary.modified <- read.csv("C:/Users/yasmine.mohamed/Google Drive/PhD Thesis/WRRI_data/Raw Data/Data_NOAA_summary modified.csv", na.strings="..")

## laptop source
Data_NOAA_summary.modified <- read.csv("C:/Users/ALMAGRABY/Google Drive/PhD Thesis/WRRI_data/Raw Data/Data_NOAA_summary modified.csv", na.strings="..")

## view data
View(Data_NOAA_summary.modified)


## summary statistics on wide format
sum1=summary (Data_NOAA_summary.modified, maxsum=125)
write.csv(sum1, file = "sum1.csv")


## reshape Data_NOAA_summary.modified  from short to long format
install.packages('reshape')
library(reshape)
noaal <- melt(Data_NOAA_summary.modified, id=c("station","Years"))
View(noaal)
fix(noaal)
View(noaal)
names(noaal)


############################################
### Merging station data with thier location
###########################################

## import stations location
Noaa.station.locations <- read.csv("C:/Users/yasmine.mohamed/Google Drive/PhD Thesis/WRRI_data/Raw Data/Noaa station locations.csv", na.strings="")

##laptopsource
Noaa.station.locations <- read.csv("C:/Users/ALMAGRABY/Google Drive/PhD Thesis/WRRI_data/Raw Data/Noaa station locations.csv")

View(Noaa.station.locations)

## change varaible name to be unified
fix(Noaa.station.locations)
View(Noaa.station.locations)


#####merge noaa station data with thier location
noaaltotal<- merge(noaal,Noaa.station.locations,by="station",all=TRUE)
noaaltotal<- join(noaal,Noaa.station.locations,by="station", match = "all")
View(noaaltotal)

##### save data file and fill in the missing of the two stations
write.csv(noaaltotal, file = "noaatotalm.csv" )
noaaltotal <- read.csv("C:/Users/ALMAGRABY/Google Drive/PhD Thesis/WRRI_data/R script/Ethiopia/noaatotalm.csv")


## change varaible name 
fix(noaaltotal)
names(noaaltotal)


##Obtain the rain by dividing rai/ 10 
noaaltotal$rain=noaaltotal$rain/10
View(noaaltotal)

## Reordering Variables in the dataframe
noaaltotal<-noaaltotal[c("station", "year", "month","rain", "LAT", "LON","ALT", "COUNTRY", "NOAA_ID")]

##sort by station  Years	
noaaltotals <- noaaltotal [order(noaaltotal$station, noaaltotal$year),]
View(noaaltotals)
names(noaaltotals)
fix(noaaltotals)

noaaltotals <-save(noaaltotals, file = "noaaltotals.RData")
load("noaaltotals.RData")


##################################################################
### Summary statistics for long form data together with location
####################################################################

## export output and output is appended
sink("output.txt", append=TRUE, split=TRUE)


## Summary statistics 
sum2=summary (noaaltotal, maxsum=125)
write.csv(sum2, file = "sum2.csv")



## Obtaining basic descriptive statitics (item name ,item number, nvalid, mean, sd, median, mad, min, max, skew, kurtosis, se)
install.packages('psych')
library(psych)
describe(noaaltotals, na.rm=TRUE,skew=FALSE,ranges=FALSE)

## Obtaining basic descriptive statitics by month
describe.by(noaaltotals, noaaltotals$month,na.rm=TRUE,skew=FALSE,ranges=FALSE)


## Obtaining basic descriptive statitics by station
describeBy(noaaltotals, noaaltotals$station,na.rm=TRUE,skew=FALSE,ranges=FALSE)

## Obtaining basic descriptive statitics by year
describeBy(noaaltotals, noaaltotals$year,na.rm=TRUE,skew=FALSE,ranges=FALSE)

# Subsetting  ADAMITULU station but I didnt use this subsetting because Total appear in the boxplot
ADAMITULU <- noaaltotals[ which(noaaltotals$station=='ADAMITULU'& !noaaltotals$month== 'Total'), ]
ADAMITULU <- ADAMITULU [order(ADAMITULU$station, ADAMITULU$year),]
View(ADAMITULU)
names(ADAMITULU)

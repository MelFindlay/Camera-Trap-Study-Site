rm(list=ls())
########################################################
#Freq histograms of activity around daw and dusk, 
#and extract summary data
#########################################################

######create ss and sr calculator function##############
#WORKS IN UTM WHICH IS SAME AS GMT
#CALL AS times<-getSunTimes()

#HERE IS FUNCTION
getSunTimes<-function(){

# load dependencies
library(rgdal)
library(maptools)

# set up variables for coordinate conversion

ukgrid ="+init=epsg:27700"
latlong ="+init=epsg:4326"

# read in csv data 'master' containing Easting, Northing and Date
df<-read.csv(file.choose(), header=TRUE)
df<-na.omit(df)

# convert dates to POSIXct-note year is in upper case, lower case used if just two numbers used for year i.e.y for 16 Y for 2016
df$Date<-as.POSIXct(as.character(df$Date),format="%d/%m/%Y")

# convert coordinates to lat long
# extract coords
coords <-cbind(Easting =as.numeric(as.character(df$Easting)), Northing =as.numeric(as.character(df$Northing)))
# convert to SP
coords_BNG<-SpatialPoints(coords, proj4string=CRS(ukgrid))
# convert to Lat Long
coords_LL<-spTransform(coords_BNG, CRS(latlong))
# update column names
colnames(coords_LL@coords)[colnames(coords_LL@coords)=="Easting"]<-"Longitude"
colnames(coords_LL@coords)[colnames(coords_LL@coords)=="Northing"]<-"Latitude"

# derive times of sunrise and sunset
# sunrise times
sr<-sunriset(coords_LL, df$Date, direction="sunrise", POSIXct.out=TRUE)
# sunset times
ss<-sunriset(coords_LL, df$Date, direction="sunset", POSIXct.out=TRUE)

# return new dataframe with added sunrise and set times
output<-cbind(df, sr$time, ss$time)
names(output)[4] <- "Sunrise"
names(output)[5] <- "Sunset"

return(output)

}
###############END OF FUNCTION######################

######################################################
#TO LOOK AT FREQUENCY HISTOGRAM OF ACTIVITY TIMES AROUND SS AND SR
#######################################################

require(rgeos)
rm(list=ls())
main<-getSunTimes()  # HERE READ IN "5_DUTY.csv" 

#CREATE VARIABLES BY SUBTRACTING EVENT FROM SS AND SR
#FIRST CHECK COLUMNS 
head(main,n=6)
#SO CAN REFER TO COlUMN NUMBER
#IF SUBSETTED DATA, CHANGE NAME OF OBJECT TO REFLECT SUBSET

SSdiff<-as.POSIXct(main$EVENT,format="%d/%m/%Y %H:%M")-as.POSIXct(main[,10],format="%Y-%m-%h %H:%M:%S")
SRdiff<-as.POSIXct(main$EVENT,format="%d/%m/%Y %H:%M")-as.POSIXct(main[,9],format="%Y-%m-%h %H:%M:%S")

#APPLY FILTER
filter<-abs(as.numeric(SRdiff))<abs(as.numeric(SSdiff))

#NEW VARIABLES
hours.sunrise<-as.numeric(SRdiff)[filter==TRUE]/3600 #saving variables for ease of use
hours.sunset<-as.numeric(SSdiff)[filter==FALSE]/3600 #saving variables for ease of use

#PLOT HISTOGRAM
hist(as.numeric(SRdiff)[filter==TRUE]/3600,breaks=20,main=NULL,
     col="grey",xlab="Hours relative to sunrise",las=1)
abline(v=0,col="black",cex=4,lwd=4)

hist(as.numeric(SSdiff)[filter==FALSE]/3600,breaks=20,main=NULL,col="grey",
xlab="Hours relative to sunset",las=1)     
abline(v=0,col="black",cex=4,lwd=4)

#COMBINED
nocturnal<-length(hours.sunset[hours.sunset>0])+length(hours.sunrise[hours.sunrise<0])
nocturnal.plushour<-length(hours.sunset[hours.sunset>-1])+length(hours.sunrise[hours.sunrise<1])
total.events<-length(hours.sunset)+length(hours.sunrise)

# PERCENTAGE OF TRULY NOCTURNAL EVENTS (i.e. BETWEEN SUNSET AND SUNRISE)
nocturnal/total.events*100

# PERCENTAGE OF EVENTS BETWEEN 1 h BEFORE SUNSET AND 1 h AFTER SUNRISE
nocturnal.plushour/total.events*100

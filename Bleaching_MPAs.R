# bleaching data with mpa data

rm(list=ls())

# overlay mpa data with bleaching and human pop
library(rgdal)
library(tidyverse)

# mpa directory

mpashpwd<-"C:/Users/40274182/OneDrive - Queen's University Belfast/PhD/Chapters/Bleaching/MPA/Data_files/mpa shp"
setwd(mpashpwd)

mpafile <- readOGR(file.path(mpashpwd,"WDPA_Apr2020_marine-shapefile-polygons.shp"))

# bleaching directory

Bleaching_data_directory="C:/Users/40274182/OneDrive - Queen's University Belfast/PhD/Chapters/Bleaching/MPA/Data_files"
setwd(Bleaching_data_directory)
Bleaching_Data <- read.csv("RC_ERG_SST_DHW_kd490.csv")

Bleaching_Data <- select(Bleaching_Data, -c(X, X.1))

coordinates(Bleaching_Data)<- ~Longitude.Degrees+Latitude.Degrees
proj4string(Bleaching_Data)<-"+proj=longlat +ellps=WGS84 +datum=WGS84"
bldata<-spTransform(Bleaching_Data,proj4string(mpafile))

test<-over(bldata,mpafile)
head(test)
test<-cbind(Bleaching_Data,test)

Bleaching_Data <- as.data.frame(test)

# na indicates the site does not fall within a protected area recognised by the IUCN 


Bleaching_Data[mapply(is.na, Bleaching_Data)] <- "Not MPA"
head(Bleaching_Data)

# excluBleaching_Datae mpa Bleaching_Dataata by year
summary(Bleaching_Data$STATUS_YR)
Bleaching_Data$STATUS_YR[Bleaching_Data$STATUS_YR=="Not MPA"]<- 1
head(Bleaching_Data$STATUS_YR)
tail(Bleaching_Data$STATUS_YR)
#excluBleaching_Datae years which are 0s
Bleaching_Data$STATUS_YR[Bleaching_Data$STATUS_YR == 0] <- NA

#mtransform mpa Bleaching_Dataata which are NA to workable format specific for each column - NA signifies site Bleaching_Dataoes not fall within an mpa, therefore shoulBleaching_Data be analyseBleaching_Data as a non protecteBleaching_Data area

Bleaching_Data$STATUS_YR[Bleaching_Data$STATUS_YR>Bleaching_Data$Year] <- NA
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$STATUS_YR),]
Bleaching_Data$STATUS_YR
Bleaching_Data$Year
Bleaching_Data$STATUS_YR[Bleaching_Data$STATUS_YR == 1] <- "Not MPA"
Bleaching_Data$STATUS_YR


write.csv(Bleaching_Data, file = "Bleaching_MPA.csv")

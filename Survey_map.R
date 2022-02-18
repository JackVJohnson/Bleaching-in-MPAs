

library(tidyverse)
library(rgdal)
library(RColorBrewer)
library(plotrix)
library(GISTools)
library(viridis)
library(Cairo)


worldmapwd<-"C:/Users/40274182/OneDrive - Queen's University Belfast/PhD/world_shape_files"

setwd(worldmapwd)
wlrd.p <- readOGR(file.path(worldmapwd,'TM_WORLD_BORDERS_SIMPL_PC150.shp'))

Bleaching_data_directory="C:/Users/40274182/OneDrive - Queen's University Belfast/PhD/Chapters/Bleaching/MPA/Data_files"
Output_directory="C:/Users/40274182/OneDrive - Queen's University Belfast/PhD/Chapters/Bleaching/MPA/Data_files/Output_directory"

setwd(Bleaching_data_directory)
Bleaching_Data <- read.csv("Bleaching_MPA.csv")

Bleaching_Data$average_bleaching <- Bleaching_Data$Count/4
Bleaching_Data$average_bleaching <- round(Bleaching_Data$average_bleaching)


Bleaching_Data$average_bleaching <- Bleaching_Data$Count/4
Bleaching_Data$average_bleaching <- round(Bleaching_Data$average_bleaching)

Bleaching_Data$SSTA_DHW <- as.numeric(Bleaching_Data$SSTA_DHW)
Bleaching_Data$SSTFilled <- as.numeric(Bleaching_Data$SSTFilled)
Bleaching_Data$SSTFilled <- Bleaching_Data$SSTFilled-273

Bleaching_Data$STATUS_YR <- as.numeric(Bleaching_Data$STATUS_YR)

Bleaching_Data$MPA[Bleaching_Data$STATUS_YR>1] <- "MPA"
Bleaching_Data$MPA[is.na(Bleaching_Data$STATUS_YR)] <- "Non MPA"
Bleaching_Data$MPA <- as.factor(Bleaching_Data$MPA)
levels(Bleaching_Data$MPA)

Bleaching_Data$MPA_AGE <- Bleaching_Data$Year-Bleaching_Data$STATUS_YR
Bleaching_Data$MPA_AGE[is.na(Bleaching_Data$MPA_AGE)] <- "Non MPA"

Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$SSTA_DHW),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$SSTFilled),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$average_bleaching),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$MPA),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$MARINE),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$MPA_AGE),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$REP_M_AREA),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$GIS_M_AREA),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$REP_AREA),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$GIS_AREA),]
Bleaching_Data <- Bleaching_Data[!is.na(Bleaching_Data$NO_TK_AREA),]

Bleaching_Data <-Bleaching_Data %>%
  dplyr::distinct(Date, Site, Region, Depth, Longitude.Degrees, Latitude.Degrees, .keep_all=T)

df2 <- dplyr::select(Bleaching_Data, c(SSTA_DHW, REP_M_AREA,  REP_AREA, MPA_AGE, GIS_AREA, GIS_M_AREA, average_bleaching, Ecoregion, Site, Region))
df2[] <- lapply(df2, gsub, pattern='Not MPA', replacement= NA)
df2[] <- lapply(df2, gsub, pattern='Non MPA', replacement= NA)

unique(df2$Site)
unique(df2$Region)

df2 <- na.omit(df2)

#########################
############## map ######

Bleachmpa=subset(Bleaching_Data, MPA == "MPA")
Bleachnon=subset(Bleaching_Data, MPA == "Non MPA")

Bleachnon$freq <- 1
dfnon <- aggregate(Bleachnon$freq, by=list(Site=Bleachnon$Site, Lat=Bleachnon$Latitude.Degrees, Lon=Bleachnon$Longitude.Degrees), FUN=sum)

Bleachmpa$freq <- 1
dfmpa <- aggregate(Bleachmpa$freq, by=list(Site=Bleachmpa$Site, Lat=Bleachmpa$Latitude.Degrees, Lon=Bleachmpa$Longitude.Degrees), FUN=sum)



windowsFonts(Arial=windowsFont("TT Arial"))

par(family="Arial")
pal<-viridis(7)
tiff(file=file.path(Output_directory,'Figure_1_map.tiff'),height=1800,width=3500,res=300)
par(mfrow=c(2,1), mgp=c(0.5,0.6,0), mar=c(1,1,1,1))
plot(wlrd.p,ylim=c(-4400000,4400000),xlim=c(-2000000,2000000), col='grey90',border='grey70')
title(main="a",adj=0)
axis(1,at=c(-10018754.17,3339584.724,16697920),lab=c('60°','180°','-60° '),las=1,tcl=0.35,mgp=c(-1,-1.3,0))
axis(2, at=c(23*111319.4666666667,0,-23*111319.4666666667),labels=c('23°','0°','-23°'),las=3,tcl=0.35,mgp=c(-2,-1.3,0),hadj=.4)
axis(3,at=c(-10018754.17,3339584.724,16697920),lab=c('','',''),las=1,tcl=0.35,mgp=c(-1,-1.3,0))
axis(4, at=c(23*111319.4666666667,0,-23*111319.4666666667),labels=c('','',''),las=2,tcl=0.35,mgp=c(-1,-0.6,0),hadj=0)
box()
xy <- dfnon[dfnon$x == 0,c('Lon','Lat')]
xy <- SpatialPointsDataFrame(dfnon=xy,coords=xy[c('Lon','Lat')], proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
xy <- spTransform(xy,CRS("+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=150 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
points(xy, cex=.7)
temp <- subset(dfnon, x > 0)
temp <- temp[with(temp, order(temp$x)),]
xy <- temp[c('Lon','Lat')]
xy <- SpatialPointsDataFrame(data=xy,coords=xy[c('Lon','Lat')], proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
xy <- spTransform(xy,CRS("+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=150 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
points(xy, cex=.7)
points(xy, cex=.7, pch=19, col=pal[temp$x])
text(-7868896,-2922012,'Indian Ocean',cex=.8)
text(9438742,487176,'Pacific Ocean',cex=.8)
GISTools::north.arrow(x=(-16654136+111319.4*320), y=1615153*2, len=(111319.4*2), lab="N", cex=.7)
#legend
plotrix::color.legend(9684797.171+25e5,-28*111319.4666666667,15807371.62+25e5,-23.5*111319.4666666667,legend=c(0,40),rect.col=c("white",pal),cex=1)
text(((15807371.62+25e5)-(9684797.171+25e5))/2+(9684797.171+25e5),-18*111319.4666666667,"Number of Surveys", cex=.75)

# mpa
plot(wlrd.p,ylim=c(-4400000,4400000),xlim=c(-2000000,2000000), col='grey90',border='grey70')
title(main="b",adj=0)
axis(1,at=c(-10018754.17,3339584.724,16697920),lab=c('60°','180°','-60° '),las=1,tcl=0.35,mgp=c(-1,-1.3,0))
axis(2, at=c(23*111319.4666666667,0,-23*111319.4666666667),labels=c('23°','0°','-23°'),las=3,tcl=0.35,mgp=c(-2,-1.3,0),hadj=.4)
axis(3,at=c(-10018754.17,3339584.724,16697920),lab=c('','',''),las=1,tcl=0.35,mgp=c(-1,-1.3,0))
axis(4, at=c(23*111319.4666666667,0,-23*111319.4666666667),labels=c('','',''),las=2,tcl=0.35,mgp=c(-1,-0.6,0),hadj=0)
box()
xy <- dfmpa[dfmpa$x == 0,c('Lon','Lat')]
xy <- SpatialPointsDataFrame(dfmpa=xy,coords=xy[c('Lon','Lat')], proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
xy <- spTransform(xy,CRS("+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=150 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
points(xy, cex=.7)
temp <- subset(dfmpa, x > 0)
temp <- temp[with(temp, order(temp$x)),]
xy <- temp[c('Lon','Lat')]
xy <- SpatialPointsDataFrame(data=xy,coords=xy[c('Lon','Lat')], proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
xy <- spTransform(xy,CRS("+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=150 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
points(xy, cex=.7)
points(xy, cex=.7, pch=19, col=pal[temp$x])
text(-7868896,-2922012,'Indian Ocean',cex=.8)
text(9438742,487176,'Pacific Ocean',cex=.8)
GISTools::north.arrow(x=(-16654136+111319.4*320), y=1615153*2, len=(111319.4*2), lab="N", cex=.7)
#legend
plotrix::color.legend(9684797.171+25e5,-28*111319.4666666667,15807371.62+25e5,-23.5*111319.4666666667,legend=c(0,40),rect.col=c("white",pal),cex=1)
text(((15807371.62+25e5)-(9684797.171+25e5))/2+(9684797.171+25e5),-18*111319.4666666667,"Number of Surveys", cex=.75)

dev.off()

df3 <- dplyr::select(Bleaching_Data, c(Ecoregion, Site, MPA)))

df_mpa <- subset(df3, MPA == "MPA", select = 1:3)
df_nonmpa <- subset(df3, MPA == "Non MPA", select = 1:3)

freqmpa <- data.frame(df_mpa$Ecoregion)
freqnon <- data.frame(df_nonmpa$Ecoregion)

freqmpa <- as.data.frame(table(freqmpa))
freqnon <- as.data.frame(table(freqnon))

colnames(freqmpa)[1] <- "Ecoregion"
colnames(freqmpa)[2] <- "MPA sites"
colnames(freqnon)[1] <- "Ecoregion"
colnames(freqnon)[2] <- "non-MPA sites"
tableS2 <- left_join(freqnon, freqmpa)
setwd(Output_directory)
write.csv(tableS2, file = "ERG_table.csv", row.names = F)


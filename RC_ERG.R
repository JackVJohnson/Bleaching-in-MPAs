# extract ecoregions for reef check data 

library(rgdal)

# rc data 

Bleaching_data_directory="C:/Users/40274182/OneDrive - Queen's University Belfast/PhD/Chapters/Bleaching/Mangroves/Data files"setwd(RC_data_directory)

Bleaching_Data <- read.csv("Reef_Check.csv")

# ecoregion wd

Ecoregion_directory <- "C:/Users/Jack V Johnson/Desktop/ERG_shp_files"
setwd(Ecoregion_directory)

# open ecoregion

ECO<-readOGR("ecoregion_exportPolygon.shp")

# get rid of holes in shp 

ecos_list<-c()
for (i in 1:150){
  eco_i<-Polygons((Filter(function(f){f@ringDir==1}, ECO@polygons[[i]]@Polygons)), ID=i)
  ecos_list<-append(ecos_list, values=eco_i, after = length(ecos_list))
  #include a brief pause (the Sys.sleep function) because if running in Rstudio, it takes a while for the code to run and for the value to be loaded into the global environment. If there is no pause, the next iteration of the loop starts before the previous value is fully saved and loaded into the environment, and there can be errors in the shapefiles 
  Sys.sleep(.2)
}
ecos<-SpatialPolygons(ecos_list)
ecos$ERG<-ECO$ERG
ecos$Ecoregion<-ECO$Ecoregion
ecos@proj4string<-ECO@proj4string
ecos@plotOrder<-ECO@plotOrder
ecos@data<-ECO@data
ECO<-ecos

# overlay 

coordinates(Bleaching_Data)<- ~Longitude.Degrees+Latitude.Degrees
proj4string(Bleaching_Data)<-"+proj=longlat +ellps=WGS84 +datum=WGS84"
bldata<-spTransform(Bleaching_Data,proj4string(ECO))

test<-over(bldata,ECO)
Bleaching_Data$Ecoregion <- as.character(test$ERG)

Bleaching_Data <- as.data.frame(Bleaching_Data)

# save file in study directory 

write.csv(Bleaching_Data, file = "RC_ERG.csv")

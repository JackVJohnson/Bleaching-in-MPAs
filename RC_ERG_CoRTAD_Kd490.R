rm(list =ls())

################################################################################################################
# code modified from Sully and van Woesik 2020 paper https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.14948 
# https://github.com/InstituteForGlobalEcology/Turbid-reefs-moderate-coral-bleaching-under-climate-related-temperature-stress/blob/master/01_Get_Turbidity_Data_final.Rmd
################################################################################################################

#load library
library(ncdf4)
library(stringr)
library(dplyr)

#set working directories
turbidity_directory = "C:/Users/40274182/OneDrive - Queen's University Belfast/PhD/Turbidity_data"
Bleaching_data_directory="C:/Users/40274182/OneDrive - Queen's University Belfast/PhD/Chapters/Bleaching/Mangroves/Data files"

#set bleaching wd
setwd(Bleaching_data_directory)

#read in belaching data 
Bleaching_Data <- read.csv(file="RC_ERG_SST_DHW_Mangrove.csv", header = TRUE, sep=",")


#set turbidity wd
setwd(turbidity_directory)
#read in turbidity data
turbidity<-nc_open("MODIS_Aqua_Monthly_4km_KD490_2002_07.nc", write=FALSE, readunlim=TRUE, verbose=FALSE)


number_of_surveys <- dim(Bleaching_Data)[1]


#calculate latitude grid cell
turbidity$var$Kd_490$dim[[2]]
turbidity$var$Kd_490$dim[[2]]$vals

lat_step <- turbidity$var$Kd_490$dim[[2]]$vals[1]-turbidity$var$Kd_490$dim[[2]]$vals[2]

Bleaching_turbidity_lat_cell<-array(0, dim=number_of_surveys)

for (i in 1:number_of_surveys)
{
  lat_grid_cell<-NA
  
  if(is.na(Bleaching_Data$Lat[i]))
  {lat_grid_cell<-NA}else{
    n_lat_steps<-floor((turbidity$var$Kd_490$dim[[2]]$vals[1]-Bleaching_Data$Lat[i])/lat_step+1)
    if((turbidity$var$Kd_490$dim[[2]]$vals[n_lat_steps]+lat_step)>=Bleaching_Data$Lat[i])
    {
      if((turbidity$var$Kd_490$dim[[2]]$vals[n_lat_steps]-lat_step)<=Bleaching_Data$Lat[i])
      {lat_grid_cell<-n_lat_steps}
      else
      {
        repeat{
          n_lat_steps=n_lat_steps+1
          if((turbidity$var$Kd_490$dim[[2]]$vals[n_lat_steps]-lat_step)<=Bleaching_Data$Lat[i]){
            if((turbidity$var$Kd_490$dim[[2]]$vals[n_lat_steps]+lat_step)>=Bleaching_Data$Lat[i])
            {break}
          }
        }
        lat_grid_cell<-n_lat_steps
      }
      
    }
    
    if((turbidity$var$Kd_490$dim[[2]]$vals[n_lat_steps]+lat_step)<Bleaching_Data$Lat[i])
    {
      repeat{
        n_lat_steps=n_lat_steps-1
        if((turbidity$var$Kd_490$dim[[2]]$vals[n_lat_steps]+lat_step)>=Bleaching_Data$Lat[i])
        {
          if((turbidity$var$Kd_490$dim[[2]]$vals[n_lat_steps]-lat_step)<=Bleaching_Data$Lat[i])
          {break}
        }
      }
      lat_grid_cell<-n_lat_steps
    }
  }
  Bleaching_turbidity_lat_cell[i]<-lat_grid_cell
} 

#calculate longitude grid cell
turbidity$var$Kd_490$dim[1]
turbidity$var$Kd_490$dim[[1]]$vals

lon_step<-turbidity$var$Kd_490$dim[[1]]$vals[2]-turbidity$var$Kd_490$dim[[1]]$vals[1]

Bleaching_turbidity_lon_cell<-array(0, dim=number_of_surveys)

for (i in 1:length(Bleaching_Data$Lon))
{
  lon_grid_cell<-NA
  if(is.na(Bleaching_Data$Lon[i]))
  {lon_grid_cell<-NA}else{
    n_lon_steps<-floor(-1*(turbidity$var$Kd_490$dim[[1]]$vals[1]-Bleaching_Data$Lon[i])/lon_step+1)
    if(n_lon_steps>(length(turbidity$var$Kd_490$dim[[1]]$vals))){n_lon_steps<-(length(turbidity$var$Kd_490$dim[[1]]$vals))}
    if(n_lon_steps<1){n_lon_steps<-1}
    if((turbidity$var$Kd_490$dim[[1]]$vals[n_lon_steps])+lon_step>=Bleaching_Data$Lon[i])
    {
      if((turbidity$var$Kd_490$dim[[1]]$vals[n_lon_steps])-lon_step<Bleaching_Data$Lon[i])
      {lon_grid_cell<-n_lon_steps}
      else
      {
        repeat{
          n_lon_steps=n_lon_steps-1
          if(n_lon_steps>(length(turbidity$var$Kd_490$dim[[1]]$vals))){n_lon_steps<-length(turbidity$var$Kd_490$dim[[1]]$vals); break}
          if((turbidity$var$Kd_490$dim[[1]]$vals[n_lon_steps])+lon_step>=Bleaching_Data$Lon[i]){
            if((turbidity$var$Kd_490$dim[[1]]$vals[n_lon_steps])-lon_step<Bleaching_Data$Lon[i])
            {break}
          }
        }
        lon_grid_cell<-n_lon_steps
      }
      
    }
    
    if((turbidity$var$Kd_490$dim[[1]]$vals[n_lon_steps])+lon_step<Bleaching_Data$Lon[i])
    {
      repeat{
        n_lon_steps=n_lon_steps+1
        if(n_lon_steps==0){n_lon_steps<-1; break}
        if((turbidity$var$Kd_490$dim[[1]]$vals[n_lon_steps])-lon_step<=Bleaching_Data$Lon[i])
        {
          if((turbidity$var$Kd_490$dim[[1]]$vals[n_lon_steps])+lon_step>Bleaching_Data$Lon[i])
          {break}
        }
      }
      lon_grid_cell<-n_lon_steps
    }
  }
  Bleaching_turbidity_lon_cell[i]<-lon_grid_cell
}

#create df

Bleaching_Data$kd490_value<-NA
setwd(turbidity_directory)

# loop

for (i in 1:dim(Bleaching_Data)[1]){
  yr<-as.character(Bleaching_Data$Year[i])
  date_string<-str_split(Bleaching_Data$Date[i], "-")
  month_string<-date_string[[1]][2]
  if (month_string=="Jan"){month<-"01"}
  if (month_string=="Feb"){month<-"02"}
  if (month_string=="Mar"){month<-"03"}
  if (month_string=="Apr"){month<-"04"}
  if (month_string=="May"){month<-"05"}
  if (month_string=="Jun"){month<-"06"}
  if (month_string=="Jul"){month<-"07"}
  if (month_string=="Aug"){month<-"08"}
  if (month_string=="Sep"){month<-"09"}
  if (month_string=="Oct"){month<-"10"}
  if (month_string=="Nov"){month<-"11"}
  if (month_string=="Dec"){month<-"12"}
  
  yr_month<-Bleaching_Data$Year[i]+(as.numeric(month)-1)/12
  if(yr_month>=2002.5 & yr_month<=2018.99){
    file_name<-paste("MODIS_Aqua_Monthly_4km_KD490_", yr, "_", month, ".nc", sep="")
    turbidity<-nc_open(file_name, write=FALSE, readunlim=TRUE, verbose=FALSE)
    turbidity_kd490<-ncvar_get(turbidity, varid="Kd_490")
    Bleaching_Data$kd490_value[i]<-turbidity_kd490[Bleaching_turbidity_lon_cell[i], Bleaching_turbidity_lat_cell[i]]
    Sys.sleep(0.2) 
    if(is.na(Bleaching_Data$kd490_value[i])){
      expand=1
      Bleaching_Data$kd490_value[i]<-mean(na.omit(as.numeric(turbidity_kd490[max(Bleaching_turbidity_lon_cell[i]-expand, 1):min(Bleaching_turbidity_lon_cell[i]+expand,dim(turbidity_kd490)[2]), max(Bleaching_turbidity_lat_cell[i]-expand,0):min(Bleaching_turbidity_lat_cell[i]+expand,max(dim(turbidity_kd490)[1]))])))
      if(is.na(Bleaching_Data$kd490_value[i])){
        expand=2
        Bleaching_Data$kd490_value[i]<-mean(na.omit(as.numeric(turbidity_kd490[max(Bleaching_turbidity_lon_cell[i]-expand, 1):min(Bleaching_turbidity_lon_cell[i]+expand,dim(turbidity_kd490)[2]), max(Bleaching_turbidity_lat_cell[i]-expand,0):min(Bleaching_turbidity_lat_cell[i]+expand,max(dim(turbidity_kd490)[1]))])))
      } #close second if is.na turbidity value
    } #close first if is.na turbidity value
    print(i)
    nc_close(turbidity)
  } #close if bleaching yr month
} #close main for loop


setwd(Bleaching_data_directory)

write.csv(Bleaching_Data, file = 'RC_ERG_SST_DHW_Mangrove_kd490.csv')

##### extract SST and DHW from CoRTAD #####


library(ncdf4)
library(stringr)
library(tidyverse)


Cortad_directory="C:/Users/Jack V Johnson/Desktop/cortad"
setwd(Cortad_directory)

Bleaching_data_directory="C:/Users/40274182/OneDrive - Queen's University Belfast/PhD/Chapters/Bleaching/Mangroves/Data files"setwd(Bleaching_data_directory)

Bleaching_Data <- read.csv("RC_ERG.csv")


#Remove NA's
Bleaching_Data=subset(Bleaching_Data, !is.na(Latitude.Degrees))
Bleaching_Data=subset(Bleaching_Data, !is.na(Longitude.Degrees))
Bleaching_Data=subset(Bleaching_Data, !is.na(Count))
Bleaching_Data=subset(Bleaching_Data, !is.na(Depth))
Bleaching_Data=subset(Bleaching_Data, !is.na(Date))
Bleaching_Data=subset(Bleaching_Data, !is.na(Ecoregion))

#CoRTAD only goes up to 2018
Bleaching_Data <- subset(Bleaching_Data, Year < 2018)
summary(Bleaching_Data$Year)

number_of_surveys=dim(Bleaching_Data)[1]

setwd(Cortad_directory)

SSTA<-nc_open("cortadv6_SSTA.nc", write=FALSE, readunlim=TRUE, verbose=FALSE)
names(SSTA$var) #prints a list of the variable names
#time_bounds, lat_bounds, lon_bounds, crs, land, NumberGood, AllBad, FilledSST, FilledSSTminimum, FilledSSTmaximum, FilledSSTstandardDeviation, FilledSSTmean
SSTA_time_bounds<-ncvar_get(SSTA, varid="time_bounds")
dim(SSTA_time_bounds) #2 1878 for ssta; Sstfilled is 2 1982
SSTA_lat_bounds<-ncvar_get(SSTA, varid="lat_bounds")
dim(SSTA_lat_bounds) #2 4320
SSTA_lon_bounds<-ncvar_get(SSTA, varid="lon_bounds")
dim(SSTA_lon_bounds) #2 8640
SSTA_land<-ncvar_get(SSTA, varid="land")
#0 or 1
dim(SSTA_land) #8640 4320


######## from here 1:42

difference<-array(0, dim=(length(SSTA_lon_bounds[1,])-1))
for (i in 1:(dim(SSTA_lon_bounds)[2]-1))
{difference[i]<-SSTA_lon_bounds[1,(i+1)]-SSTA_lon_bounds[1,i]}

Bleaching_cortad_lat_cell<-array(0, dim=number_of_surveys)
lat_step<--1*(SSTA_lat_bounds[2,dim(SSTA_lat_bounds)[2]]-SSTA_lat_bounds[1,1])/(dim(SSTA_lat_bounds)[2]+1)
for (i in 1:number_of_surveys)
{
  lat_grid_cell<-NA
  if(is.na(Bleaching_Data$Latitude.Degrees[i]))
  {lat_grid_cell<-NA}else{
    n_lat_steps<-floor((SSTA_lat_bounds[1,1]-Bleaching_Data$Latitude.Degrees[i])/lat_step+1)
    if(SSTA_lat_bounds[1,n_lat_steps]>=Bleaching_Data$Latitude.Degrees[i])
    {
      if(SSTA_lat_bounds[2,n_lat_steps]<=Bleaching_Data$Latitude.Degrees[i])
      {lat_grid_cell<-n_lat_steps}
      else
      {
        repeat{
          n_lat_steps=n_lat_steps+1
          if(SSTA_lat_bounds[1,n_lat_steps]>Bleaching_Data$Latitude.Degrees[i]){
            if(SSTA_lat_bounds[2,n_lat_steps]<=Bleaching_Data$Latitude.Degrees[i])
            {break}
          }
        }
        lat_grid_cell<-n_lat_steps
      }
      
    }
    
    if(SSTA_lat_bounds[1,n_lat_steps]<Bleaching_Data$Latitude.Degrees[i])
    {
      repeat{
        n_lat_steps=n_lat_steps-1
        if(SSTA_lat_bounds[1,n_lat_steps]>=Bleaching_Data$Latitude.Degrees[i])
        {
          if(SSTA_lat_bounds[2,n_lat_steps]<=Bleaching_Data$Latitude.Degrees[i])
          {break}
        }
      }
      lat_grid_cell<-n_lat_steps
    }
  }
  Bleaching_cortad_lat_cell[i]<-lat_grid_cell
}


Bleaching_cortad_lon_cell<-array(0, dim=number_of_surveys)
lon_step<-(SSTA_lon_bounds[1,dim(SSTA_lon_bounds)[2]]-SSTA_lon_bounds[1,1])/(dim(SSTA_lon_bounds)[2]+1)
for (i in 1:length(Bleaching_Data$Longitude.Degrees))
{
  lon_grid_cell<-NA
  if(is.na(Bleaching_Data$Longitude.Degrees[i]))
  {lon_grid_cell<-NA}else{
    n_lon_steps<-floor(-1*(SSTA_lon_bounds[1,1]-Bleaching_Data$Longitude.Degrees[i])/lon_step+1)
    if(n_lon_steps>(dim(SSTA_lon_bounds)[2])){n_lon_steps<-(dim(SSTA_lon_bounds)[2])}
    if(n_lon_steps<1){n_lon_steps<-1}
    if(SSTA_lon_bounds[1,n_lon_steps]<=Bleaching_Data$Longitude.Degrees[i])
    {
      if(SSTA_lon_bounds[2,n_lon_steps]>Bleaching_Data$Longitude.Degrees[i])
      {lon_grid_cell<-n_lon_steps}
      else
      {
        repeat{
          n_lon_steps=n_lon_steps+1
          if(n_lon_steps>(dim(SSTA_lon_bounds)[2])){break}
          if(SSTA_lon_bounds[1,n_lon_steps]<=Bleaching_Data$Longitude.Degrees[i]){
            if(SSTA_lon_bounds[2,n_lon_steps]>Bleaching_Data$Longitude.Degrees[i])
            {break}
          }
        }
        lon_grid_cell<-n_lon_steps
      }
      
    }
    
    if(SSTA_lon_bounds[1,n_lon_steps]>Bleaching_Data$Longitude.Degrees[i])
    {
      repeat{
        n_lon_steps=n_lon_steps-1
        if(n_lon_steps==0){break}
        if(SSTA_lon_bounds[1,n_lon_steps]<=Bleaching_Data$Longitude.Degrees[i])
        {
          if(SSTA_lon_bounds[2,n_lon_steps]>Bleaching_Data$Longitude.Degrees[i])
          {break}
        }
      }
      lon_grid_cell<-n_lon_steps
    }
  }
  Bleaching_cortad_lon_cell[i]<-lon_grid_cell
}

str(Bleaching_Data$Date)
summary(Bleaching_Data$Date)

Bleaching_days_since_19811231<-array(0, dim=number_of_surveys)
for (i in 1:number_of_surveys)
{
  date_string<-str_split(Bleaching_Data$Date[i], "-")
  day_string<-date_string[[1]][1]
  day_numeric<-as.numeric(day_string)
  month_string<-date_string[[1]][2]
  if (month_string=="Jan"){days_since_19811231_due_to_month_number<-0}
  if (month_string=="Feb"){days_since_19811231_due_to_month_number<-31}
  if (month_string=="Mar"){days_since_19811231_due_to_month_number<-59}
  if (month_string=="Apr"){days_since_19811231_due_to_month_number<-90}
  if (month_string=="May"){days_since_19811231_due_to_month_number<-120}
  if (month_string=="Jun"){days_since_19811231_due_to_month_number<-151}
  if (month_string=="Jul"){days_since_19811231_due_to_month_number<-181}
  if (month_string=="Aug"){days_since_19811231_due_to_month_number<-212}
  if (month_string=="Sep"){days_since_19811231_due_to_month_number<-243}
  if (month_string=="Oct"){days_since_19811231_due_to_month_number<-273}
  if (month_string=="Nov"){days_since_19811231_due_to_month_number<-304}
  if (month_string=="Dec"){days_since_19811231_due_to_month_number<-334}
  year_string<-date_string[[1]][3]
  year_numeric<-as.numeric(year_string)
  century<-1900
  if(year_numeric<25) #ex. 95 would mean 1995, 25 would mean 2025
  {century<-2000}
  full_year<-century+year_numeric
  #add in the number of leap years
  #the following are leap years: 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016
  leap_year_days<-0
  if( (full_year>1984) | (full_year==1984 & month_string!="Jan" & month_string!="Feb"))
  {leap_year_days<-1}
  if( (full_year>1988) | (full_year==1988 & month_string!="Jan" & month_string!="Feb"))
  {leap_year_days<-2}
  if( (full_year>1992) | (full_year==1992 & month_string!="Jan" & month_string!="Feb"))
  {leap_year_days<-3}
  if( (full_year>1996) | (full_year==1996 & month_string!="Jan" & month_string!="Feb"))
  {leap_year_days<-4}
  if( (full_year>2000) | (full_year==2000 & month_string!="Jan" & month_string!="Feb"))
  {leap_year_days<-5}
  if( (full_year>2004) | (full_year==2004 & month_string!="Jan" & month_string!="Feb"))
  {leap_year_days<-6}
  if( (full_year>2008) | (full_year==2008 & month_string!="Jan" & month_string!="Feb"))
  {leap_year_days<-7}
  if( (full_year>2012) | (full_year==2012 & month_string!="Jan" & month_string!="Feb"))
  {leap_year_days<-8}
  if( (full_year>2016) | (full_year==2016 & month_string!="Jan" & month_string!="Feb"))
  {leap_year_days<-9}
  days_since_19811231<-((full_year-1982)*365)+days_since_19811231_due_to_month_number+day_numeric+leap_year_days
  Bleaching_days_since_19811231[i]<-days_since_19811231
}

Bleaching_cortad_day_index<-array(0, dim=number_of_surveys)
max_index_of_CoRTAD<-dim(SSTA_time_bounds)[2]+1
for (i in 1:number_of_surveys)
{
  Bleaching_cortad_day_index[i]<-floor((Bleaching_days_since_19811231[i]+SSTA_time_bounds[1,1])/7)+1
  if (Bleaching_cortad_day_index[i]>max_index_of_CoRTAD){Bleaching_cortad_day_index[i]<-NA}
}

# create arrays to fill data with 

Bleaching_cortad_SSTA_DHW<-array(NA, dim=number_of_surveys)
Bleaching_cortad_SSTFilled<-array(NA, dim=number_of_surveys)

#FilledSST, FilledSSTminimum, FilledSSTmaximum, FilledSSTstandardDeviation, FilledSSTmean
SSTA<-nc_open("cortadv6_SSTA.nc", write=FALSE, readunlim=TRUE, verbose=FALSE)
names(SSTA$var) #SSTA_Minimum, SSTA_Maximum, SSTA_StandardDeviation, SSTA_Mean, SSTA_AbsoluteValueMean, SSTA_Frequency, SSTA_FrequencyMax, SSTA_FrequencyStandardDeviation, SSTA_FrequencyMean, SSTA_DHW,  SSTA_DHWMax, SSTA_DHWStandardDeviation, SSTA_DHWMean
FilledSST<-nc_open("cortadv6_FilledSST.nc", write=FALSE, readunlim=TRUE, verbose=FALSE)
names(FilledSST$var)

# create functions

first_pass_function_3d<-function(netcdf_variable_name, variable_id){
  result<-try(ncvar_get(netcdf_variable_name, varid=variable_id, start=c(Bleaching_cortad_lon_cell[i],Bleaching_cortad_lat_cell[i],Bleaching_cortad_day_index[i]), count=c(1,1,1)), silent=TRUE)
  return(result)
}

second_pass_3d<-function(netcdf_variable_name, variable_id){
  expand=1
  result=NA
  repeat{
    expanded_grid<-try(ncvar_get(netcdf_variable_name, varid=variable_id, start=c((Bleaching_cortad_lon_cell[i]-expand),(Bleaching_cortad_lat_cell[i]-expand),Bleaching_cortad_day_index[i]), count=c((1+2*expand),(1+2*expand),1)), silent=TRUE)
    
    if(sum(is.na(expanded_grid))==((1+2*expand)*(1+2*expand)))
    {expand=expand+1
    if (expand>=3){break}
    }
    else{
      result<-mean(expanded_grid, na.rm=TRUE)
      break}
  }
  return(result)
}


# get


for (i in 1:number_of_surveys)
{
  if(!is.na(Bleaching_cortad_day_index[i]))
  {
    if(!is.na(Bleaching_cortad_lon_cell[i]))
    {
      if(!is.na(Bleaching_cortad_lat_cell[i]))
      {
        Bleaching_cortad_SSTA_DHW[i]<-first_pass_function_3d(SSTA, "SSTA_DHW")
        Bleaching_cortad_SSTFilled[i]<-first_pass_function_3d(FilledSST, "FilledSST")
      }
    }
  }
  print(i)
}


Bleaching_cortad_SSTA_DHW<-as.numeric(Bleaching_cortad_SSTA_DHW)

for (i in 1:number_of_surveys)
{
  if(!is.na(Bleaching_cortad_lon_cell[i]))
  {
    if(!is.na(Bleaching_cortad_lat_cell[i]))
    {
      if(!is.na(Bleaching_cortad_day_index[i]))
      {
        if(is.na(Bleaching_cortad_SSTA_DHW[i]))
        {Bleaching_cortad_SSTA_DHW[i]<-second_pass_3d(SSTA, "SSTA_DHW")}
        if(is.na(Bleaching_cortad_SSTFilled[i]))
        { Bleaching_cortad_SSTFilled[i]<-second_pass_3d(FilledSST, "FilledSST")}
        print(i)
      } #close lon
    } #close lat
  } #close day index
} #close number_of_surveys

setwd(Bleaching_data_directory)



Bleaching_Data_with_cortad_variables <- cbind(Bleaching_Data, Bleaching_cortad_SSTA_DHW, Bleaching_cortad_SSTFilled)

number_of_columns<-dim(Bleaching_Data)[2]

colnames(Bleaching_Data_with_cortad_variables)[number_of_columns+1]<-"SSTA_DHW"
colnames(Bleaching_Data_with_cortad_variables)[number_of_columns+2]<-"SSTFilled"

write.csv(Bleaching_Data_with_cortad_variables, file = "RC_ERG_SST_DHW.csv")

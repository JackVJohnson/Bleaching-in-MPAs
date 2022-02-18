# Bleaching-in-MPAs
Code runs in the following order. Skip to step 6 if looking to just re-run analyses.
1.	RC_ERG_DHW combines DHW data from CoRTAD version 6 with the bleaching and ecoregion (from Veron et al 2015) data
Files: RC_ERG.csv, Reef check data from Reefcheck.org, ecoregions available from Veron et al, DHW data available from LINK 
2.	RC_ERG_SST_DHW_Kd490
Files: RC_ERG_SST_DHW.csv with turbidity data 
3.	Bleaching_MPA
Files: https://oceandata.sci.gsfc.nasa.gov/MODIS-Aqua/Mapped/Monthly/4km/Kd_490/)
4.	RC_ERG_DHW_MPA identifies whether a reef check survey falls within or outside an MPA boundry
Files: RC_ERG_DHW.csv, MPA data available from world database of protected areas LINK
5.	Surveys_map creates Fig. 1 surveys map
Files: Bleaching_MPA.csv with world_shape_files
6.	Stan_nb creates the correlation matrix for MPA attribute data and runs the Bayesian model 
Files: Bleaching_MPA.csv 
7.	DHW_and_density_analysis runs analysis to determine differences in bleaching severity and the onset of bleaching based on temperature gamma distributions
Files: Bleaching_MPA.csv  

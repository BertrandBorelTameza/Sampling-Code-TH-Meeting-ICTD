#########################################################################################################################################################
# Purpose:                 Sampling code for the TH meeting: This script matches each hh to the nearest meeting point (TH)                              #
# Author:                  Bertrand T. Tameza                                                                                                           #
# Version:                 3.5.1 (2018-07-02)                                                                                                           #
#########################################################################################################################################################


# load the necessary libraries
library(dplyr)
library(magrittr)
library(tidyverse)

# Get polling centre dataset
Polling_centre_data = as_tibble(read.csv("sierra-leone-polling-centres.csv"))
warc_data = as_tibble(read.csv2("Warc_Sample_data.csv"))

# Polling centre in the western area urban and their gps-coordinates
Poll_cent_West_Urban = filter(Polling_centre_data,PollingCentreDistrict == "Western Area Urban")
Gps_poll_cent_west_urban = select(Poll_cent_West_Urban, PollingCentreName, PollingCentreLatitude, PollingCentreLongitude)

# head(Gps_poll_cent_west_urban)

# Households IDs and their gps-coordinates
Gps_Households = select(warc_data, propid, mark_lat, mark_long)

# Data type conversion
Gps_Households %<>% mutate(propid = as.character(propid), mark_lat = as.numeric(levels(mark_lat))[mark_lat])
Gps_poll_cent_west_urban %<>% mutate(propid = as.character(PollingCentreName))

# This function converts degree to radian
Deg2Rad = function(input){
  pi =   22/7
  Rad = (pi * input)/180
  return(Rad)
}

# Compute the distance (in meter) between 2 points knowing their GPS-coordinates (latitude, longitude)
get_distance_m = function(latitude1, longitude1, latitude2, longitude2){
  # Earth Radius from wiki
  earth_radius = 6378100
  # Conversion degree to radian
  rlat1 = Deg2Rad(latitude1)
  rlong1 = Deg2Rad(longitude1)
  rlat2 = Deg2Rad(latitude2)
  rlong2 = Deg2Rad(longitude2)
  
  dlongitude = (rlong2 - rlong1)/2
  dlatitude = (rlat2 - rlat1)/2
  
  a = sin(dlatitude)*sin(dlatitude) + cos(rlat1)*cos(rlat2)*(sin(dlongitude)*sin(dlongitude))
  
  distance = 2*atan2(sqrt(a), sqrt(1-a))
  
  return(earth_radius*distance)
  
}

# Create and fill up the matrix of distances from each household to each TH

# Create matrix of distances: A huge matrix of more than 9 million elements :)
Matrix_of_distance = matrix(data = NA, nrow = nrow(Gps_Households), ncol = nrow(Gps_poll_cent_west_urban))

# Fill up the matrix of distances
for (i in c(1:nrow(Gps_Households))){
  for (j in c(1:nrow(Gps_poll_cent_west_urban))){
    Matrix_of_distance[i,j] = get_distance_m(Gps_Households$mark_lat[i], Gps_Households$mark_long[i],
                                             Gps_poll_cent_west_urban$PollingCentreLatitude[j],
                                             Gps_poll_cent_west_urban$PollingCentreLongitude[j])
  }
    
}

# Match each household (Using their IDs) to the nearest Polling Centre (TH) in the wertern area urban

Matching_list <- setNames(data.frame(matrix(ncol = 3, nrow = nrow(Gps_Households))), c("Property ID", "Polling_Centre_Code", "Distance (m)"))
Column = vector(mode = "list", length = nrow(Gps_Households))

Matching = function(hh_data, th_data, mat_dist, match_list){
  
  for (i in c(1: nrow(hh_data))){
    match_list[i,1] = hh_data[which(mat_dist == min(mat_dist[i,]), arr.ind = T)[1],1]
    match_list[i,2] = th_data[which(mat_dist == min(mat_dist[i,]), arr.ind = T)[2],1]
    match_list[i,3] = min(mat_dist[i,])
    Column[i] = th_data[which(mat_dist == min(mat_dist[i,]), arr.ind = T)[2],1]
  }
  # show result: A dataframe of 83,362 rows and 3 columns 
  match_list
}

# make the matching : This takes a litte while to run (billions of operations and comparaisons)
Matching(Gps_Households, Gps_poll_cent_west_urban, Matrix_of_distance, Matching_list)

 

####################################################   END OF THE SCRIPT   ###############################################################################

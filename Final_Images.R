library(tidyverse)
library(stringr)     

library(sf)     
library(raster)
library(sp)
library(mapview)
library(tmap)

library(tidycensus)
library(tigris)
library(lehdr)

library(httr) 
library(jsonlite)
library(viridis)
library(ggpubr)
library(maptools)
library(rgdal)
library(rgeos)
library(treemap)

##
##
##   
##
##
##      Change the dir variable to the location that you have 
##      saved this file. Make sure the fina_data_files folder 
##      is also in this directory.


dir = 'C:/Users/dpedrick/OneDrive/GaTech/Advanced GIS/Final Project/Images'
dir2 = 'C:/Users/dpedrick/OneDrive/GaTech/Advanced GIS/Final Project'

## Load in all of the data

municipal_combined <- st_read(paste(dir,"/final_data_files/municipal_combined/municipal_parcels_final.shp",sep = '')) %>%
  st_transform(26967)

municipally_owned_parcels <- st_read(paste(dir,"/final_data_files/municipally_owned_parcels/municipal_parcels.shp",sep = ''))%>%
  st_transform(26967)

City_of_Atlanta_Limits <- st_read(paste(dir2,"/Atlanta_City_Limits/Atlanta_City_Limits.shp",sep = ''))%>%
  st_transform(26967)

atlanta_buildings <- st_read(paste(dir2,"/Building_Footprints/Building_Footprints.shp",sep = ''))%>%
  st_transform(26967)

regional_roads <- st_read(paste(dir2,"/Major_Roads/Major_Roads.shp",sep = ''))%>%
  st_transform(26967)

atlanta_parks <- st_read(paste(dir2,"/Parks/Parks.shp",sep = ''))%>%
  st_transform(26967)



atlanta_roads <- regional_roads[City_of_Atlanta_Limits,]


bus_raster <-raster(paste(dir,"/final_data_files/raster_files/bus_raster.tif",sep = ''))
econ_mobility_raster <-raster(paste(dir,"/final_data_files/raster_files/econ_mobile_raster.tif",sep = ''))
envi_hazard_raster <-raster(paste(dir,"/final_data_files/raster_files/envi_hazard_raster.tif",sep = ''))
grocery_raster <-raster(paste(dir,"/final_data_files/raster_files/grocery_raster.tif",sep = ''))
job_access_raster <-raster(paste(dir,"/final_data_files/raster_files/job_access_raster.tif",sep = ''))
parks_raster <-raster(paste(dir,"/final_data_files/raster_files/parks_raster.tif",sep = ''))
train_raster <-raster(paste(dir,"/final_data_files/raster_files/train_raster.tif",sep = ''))
transpo_raster <-raster(paste(dir,"/final_data_files/raster_files/transpo_raster.tif",sep = ''))

final_raster <-raster(paste(dir,"/final_data_files/raster_files/final_raster.tif",sep = ''))


##  Create the following maps: 
##    Lets have each of the raster maps visible in a grid on the Poster
##    I think there are 8 raster files so it could be a 2 x 4 Grid
##    Make the Final Raster file large
    
##    Then we should show how we found the municipally owned parcels
##    I think we should show all three municipally owned parcels, 
##    municipal vacant parcels, and municipal under utilized parcels
    
##    Each of those should be colored by their suitability score.
    
##    I think for the graphs we could have a historgram showing how many
##    properties score well for a few of the suitability categories.
    

vacant_land <- municipal_combined %>%
  filter(
    municipal_combined$vacant == 1
  )

under_used_land <- municipal_combined %>%
  filter(
    municipal_combined$undr_tl == 1
  )

avaliable_land <- municipal_combined %>%
  filter(
    municipal_combined$vacant == 1 | municipal_combined$undr_tl == 1
  )

tmap_mode("plot")

## Composite score
tm_shape(final_raster)+
  tm_raster("final_raster", palette= "YlGn",n=8,
            title="Composite Score") +
  tm_shape(City_of_Atlanta_Limits) +
  tm_borders(col="black", lwd = 3, alpha = 1) +
  tm_layout(legend.title.size = 1,
            legend.position = c('left','bottom'),
            legend.bg.color = 'white')


tm_shape(bus_raster)+
  tm_raster("bus_raster", palette= "YlGn",n=8,
            title="Bus Access Score") +
  tm_shape(City_of_Atlanta_Limits) +
  tm_borders(col="black", lwd = 3, alpha = 1) +
  tm_layout(legend.title.size = 1,
            legend.position = c('left','bottom'),
            legend.bg.color = 'white')

tm_shape(train_raster)+
  tm_raster("train_raster", palette= "YlGn",n=8,
            title="Train Access Score") +
  tm_shape(City_of_Atlanta_Limits) +
  tm_borders(col="black", lwd = 3, alpha = 1) +
  tm_layout(legend.title.size = 1,
            legend.position = c('left','bottom'),
            legend.bg.color = 'white')


tm_shape(transpo_raster)+
  tm_raster("transpo_raster", palette= "YlGn",n=8,
            title="Transportation Cost Score") +
  tm_shape(City_of_Atlanta_Limits) +
  tm_borders(col="black", lwd = 3, alpha = 1) +
  tm_layout(legend.title.size = 1,
            legend.position = c('left','bottom'),
            legend.bg.color = 'white')

tm_shape(econ_mobility_raster) +
  tm_raster("econ_mobile_raster", palette= "YlGn",n=8,
            title="Economic Mobility Score") +
  tm_shape(City_of_Atlanta_Limits) +
  tm_borders(col="black", lwd = 3, alpha = 1) +
  tm_layout(legend.title.size = 1,
            legend.position = c('left','bottom'),
            legend.bg.color = 'white')


tm_shape(envi_hazard_raster)+
  tm_raster("envi_hazard_raster", palette= "-YlGn",n=8,
            title="Environmental Hazard Score") +
  tm_shape(City_of_Atlanta_Limits) +
  tm_borders(col="black", lwd = 3, alpha = 1) +
  tm_layout(legend.title.size = 1,
            legend.position = c('left','bottom'),
            legend.bg.color = 'white')


tm_shape(grocery_raster)+
  tm_raster("grocery_raster", palette= "YlGn",n=8,
            title="Grocery Access Score") +
  tm_shape(City_of_Atlanta_Limits) +
  tm_borders(col="black", lwd = 3, alpha = 1) +
  tm_layout(legend.title.size = 1,
            legend.position = c('left','bottom'),
            legend.bg.color = 'white')

tm_shape(job_access_raster)+
  tm_raster("job_access_raster", palette= "YlGn",n=8,
            title="Job Access Score") +
  tm_shape(City_of_Atlanta_Limits) +
  tm_borders(col="black", lwd = 3, alpha = 1) +
  tm_layout(legend.title.size = 1,
            legend.position = c('left','bottom'),
            legend.bg.color = 'white')

tm_shape(parks_raster)+
  tm_raster("parks_raster", palette= "YlGn",n=8,
            title="Park Access Score") +
  tm_shape(City_of_Atlanta_Limits) +
  tm_borders(col="black", lwd = 3, alpha = 1) +
  tm_layout(legend.title.size = 1,
            legend.position = c('left','bottom'),
            legend.bg.color = 'white')


avaliable_land_sorted <- avaliable_land[order(-avaliable_land$fnl_scr, -avaliable_land$LndAcrs),]

Municipally_Owned_Land <-avaliable_land_sorted %>%
  mutate(
    vacant2 = ifelse(is.na(vacant),"FALSE", "TRUE"),
    undr_tl2 = ifelse(is.na(undr_tl), "FALSE", "TRUE"),
    fnl_scr2 = as.integer(fnl_scr),
    LndAcrs = ifelse(is.na(LndAcrs), 0, LndAcrs)
  )


vacant <- Municipally_Owned_Land %>%
  filter(
    vacant2 == "TRUE"
  )

tmap_mode("view")
tm_basemap("Stamen.TonerLite")+
  tm_basemap("Esri.WorldImagery")+
  tm_shape(Municipally_Owned_Land) +
  tm_polygons(col= 'fnl_scr', alpha = 0.5,
              border.alpha = 0.1, 
              palette= "YlGn",
              breaks = c(0, 1 ,2, 3, 4, 5, 6, 7, 8),
              id = "Address",
              popup.vars = c("Address"  = "Address",
                             "Owner" = "Owner",
                             "Suitability Score" = "fnl_scr",
                             "Size (acres)" = "LndAcrs",
                             "Underutilized" = "undr_tl2",
                             "Vacant" = "vacant2",
                             "Economic Mobility" ="ecn_scr",
                             "Environmnetal Hazards" = "env_scr",
                             "Grocery Access" = "grcry_s",
                             "Job Access" = "job_scr",
                             "Parks Access" = "prks_sc",
                             "Transportation Cost" = "trnsp_s",
                             "Bus Access" = "bus_scr",
                             "Train Access" = "trn_scr"),
                title="Available Parcels Suitability Score" )+
  tm_shape(City_of_Atlanta_Limits) +
  tm_borders(col="black", lwd = 3, alpha = 1) + 
  tm_view(set.view = c(-84.395805, 33.775596, 14)) 


## Sort by score and then by size
## Sum of all high scoring parcels


ggplot(data = Municipally_Owned_Land , mapping = aes(x = fnl_scr)) + 
  geom_histogram(binwidth = 0.1)

available_land_greater_5 <- Municipally_Owned_Land %>%
  filter(
    fnl_scr2 >=5
  )

ggplot(data = available_land_greater_5 , mapping = aes(x = Owner, y = LndAcrs, fill = fnl_scr2)) +
  geom_col() + coord_flip()+ theme(legend.title = element_blank())


available_land_by_owner <- available_land_greater_5 %>%
  group_by(
    Owner, 
    add = FALSE
    ) %>%
  summarise(acres = sum(LndAcrs)) %>%
  filter(
    acres > 10
  )

ggplot(data = available_land_by_owner , mapping = aes(x = Owner, y = acres)) +
  geom_col() + coord_flip()+ theme(legend.title = element_blank())


available_land_by_owner_all <- Municipally_Owned_Land %>%
  group_by(
    Owner,
    fnl_scr2,
    add = FALSE
  ) %>%
  summarise(acres = sum(LndAcrs)) %>%
  filter(
    acres > 10
  )

ggplot(data = available_land_by_owner_all , mapping = aes(x = Owner, y = acres, fill=fnl_scr2)) +
  geom_col() + coord_flip()+ theme(legend.title = element_blank())



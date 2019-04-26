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




dir = 'C:/Users/dpedrick/OneDrive/GaTech/Advanced GIS/Final Project'

# Read in data:

#Census tract level data in shapefile polygons
health <- st_read(paste(dir,"/Environmental Health Hazard Index/output.shp",sep = ''))
transpoCost <- st_read(paste(dir,"/Low_Transportation_Cost_Index/Low_Transportation_Cost_Index.shp",sep = ''))

#Census tract level data in csv format
opportunity <- read_csv(paste(dir,"/tract_outcomes/data.txt",sep = ''))
CNT_HT <- read_csv(paste(dir,"/htaindex_data_tracts_13/htaindex_data_tracts_13.csv",sep = ''))

#Polygons
city <- st_read(paste(dir,"/Atlanta_City_Limits/Atlanta_City_Limits.shp",sep = '')) %>%
  st_transform(26967)
parks <- st_read(paste(dir,"/Parks/Parks.shp",sep = ''))%>%
  st_transform(26967)

#Point files
grocery <- st_read(paste(dir,"/Grocery Stores/Atlanta_grocery_stores.shp",sep = ''))%>%
  st_transform(26967)
transit <- st_read(paste(dir,"/Transit_Stops_2018/Transit_Stops_2018.shp",sep = ''))%>%
  st_transform(26967)


# Retrieve census tracts using ##########
tracts_ga <- tracts(
  state = 13, cb = FALSE, year = 2018) %>% 
  st_as_sf() %>%
  st_transform(crs = 26967) 

# Filter to only include census tracts in Fulton and Dekalb County
tracts_atl <- tracts_ga %>%
  filter(tracts_ga$COUNTYFP == "121" | tracts_ga$COUNTYFP == "089")


## Append the census tract level data with the atlanta census tracts

# First, filter census tract level data to only include Fulton and Dekalb County
health_indicator <- health%>%
  filter(health$STATE == 13 & (as.character(health$COUNTY) == '121' | as.character(health$COUNTY) == '089')) %>%
  mutate(
    GEOID = as.character(GEOID)
  )

transpo_cost <- transpoCost%>%
  filter(transpoCost$STATE == 13 & (as.character(transpoCost$COUNTY) == '121' | as.character(transpoCost$COUNTY) == '089')) %>%
  mutate(
    GEOID = as.character(GEOID)
  ) 

# Second, remove geometry as we are using the census tract file's geometry
st_geometry(health_indicator) <- NULL
st_geometry(transpo_cost) <- NULL

# Third, mutate the csv census tract level data to make census tract column read GEOID for future joins
opportunity <- opportunity %>%
  mutate(
    GEOID = as.character(census_tract)
  )

CNT_HT2 <- CNT_HT %>%
  mutate(
    GEOID = as.character(CNT_HT$tract)
  )

# Finally, join all data together by GEOID

tracts_atl2 <- inner_join(tracts_atl, opportunity, by= c("GEOID"))
tracts_atl3 <- inner_join(tracts_atl2, CNT_HT2, by="GEOID")
tracts_atl4 <- inner_join(tracts_atl3, health_indicator, by="GEOID")
tracts_atl5 <- inner_join(tracts_atl4, transpo_cost, by="GEOID") %>%
  st_transform(26967)

# Subset the data by the city boundary
tracts_atl6 <- tracts_atl5[city,]

# Mutate all data to retrieve just the columns needed for analysis
final_tracts <- tracts_atl6 %>%
  mutate(
    land_area = ALAND,
    prob_1 = kir_pooled_pooled_p1, 
    prob_25 = kir_pooled_pooled_p25,
    prob_75 = kir_pooled_pooled_p75,
    prob_100 = kir_pooled_pooled_p100,
    prob_mean = kir_pooled_pooled_mean,
    transpo_cost = t_ami,
    compact_neighborhood= compact_ndx,
    job_access = emp_ovrll_ndx,
    envi_hazard = haz_idx,
    transpo_cost_arc = tcost_idx
  ) %>%
  select(GEOID, land_area, prob_1, prob_25, prob_75, prob_100, prob_mean, transpo_cost, compact_neighborhood, job_access, envi_hazard, transpo_cost_arc, population , households)

# Range function to scale variables between 0 and 1
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# Scale variables to be between 0 and 1
final_tracts_scaled <- tracts_atl6 %>%
  mutate(
    land_area = ALAND,
    prob_1 = range01(ifelse(is.na(kir_pooled_pooled_p1)==TRUE,0,kir_pooled_pooled_p1)), 
    prob_25 = range01(ifelse(is.na(kir_pooled_pooled_p25)==TRUE,0,kir_pooled_pooled_p25)),
    prob_75 = range01(ifelse(is.na(kir_pooled_pooled_p75)==TRUE,0,kir_pooled_pooled_p75)),
    prob_100 = range01(ifelse(is.na(kir_pooled_pooled_p100)==TRUE,0,kir_pooled_pooled_p100)),
    prob_mean = range01(ifelse(is.na(kir_pooled_pooled_mean)==TRUE,0,kir_pooled_pooled_mean)),
    transpo_cost = range01(ifelse(is.na(t_ami)==TRUE, 0, t_ami)),
    compact_neighborhood= range01(ifelse(is.na(compact_ndx)==TRUE, 0, compact_ndx)),
    job_access = range01(ifelse(is.na(emp_ovrll_ndx)==TRUE, 0, emp_ovrll_ndx)),
    envi_hazard = range01(ifelse(is.na(haz_idx)==TRUE, 0,haz_idx)),
    transpo_cost_arc = range01(ifelse(is.na(tcost_idx)==TRUE, 0, tcost_idx)),
    suitability = range01(prob_mean + (-transpo_cost) + job_access + (-envi_hazard))
  ) %>%
  select(GEOID, suitability, land_area, prob_1, prob_25, prob_75, prob_100, prob_mean, transpo_cost, compact_neighborhood, job_access, envi_hazard, transpo_cost_arc, population , households)




## Filter transit stops for just MARTA train and then for just MARTA bus
marta_train <- transit %>%
  filter(transit$agency_nam == "MARTA" & (transit$routes == "RED" | transit$routes  == "GREEN" | transit$routes =="BLUE" | transit$routes == "GOLD"  | transit$routes == "RED, GOLD" | transit$routes == "GREEN, BLUE"))
marta_bus <- transit %>%
  filter(transit$agency_nam == "MARTA" & (transit$routes != "RED" | transit$routes  != "GREEN" | transit$routes !="BLUE" | transit$routes != "GOLD"  | transit$routes != "RED, GOLD" | transit$routes != "GREEN, BLUE"))


## Create buffers for proximal weights to point and polygon data
grocery_buffer <- st_buffer(grocery, dist=800)
parks_buffer <- st_buffer(parks, dist=800)
MARTA_train_buffer <- st_buffer(marta_train, dist = 800)
MARTA_bus_buffer <- st_buffer(marta_bus, dist = 400)

## Merge (st_union) all buffers
grocery_buffer_combined_A <- st_union(grocery_buffer) %>%
  st_sf()
parks_buffer_combined_A <- st_union(parks_buffer) %>%
  st_sf()
bus_buffer_merged_A <- st_union(MARTA_bus_buffer, by_feature = FALSE)%>%
  st_sf()
train_buffer_merged_A <- st_union(MARTA_train_buffer, by_feature = FALSE)%>%
  st_sf()

# Subset by city limits
grocery_buffer_combined <- grocery_buffer_combined_A[city,]
parks_buffer_combined <- parks_buffer_combined_A[city,] 
bus_buffer_merged <- bus_buffer_merged_A[city,]
train_buffer_merged <- train_buffer_merged_A[city,]

# Convert all buffers to rasters
raster_template  <- raster(extent(city), resolution = 200, crs = st_crs(city)$proj4string)
grocery_raster <- raster::rasterize(grocery_buffer_combined, raster_template, field = 1, fun=sum, background = 0)
parks_raster <- raster::rasterize(parks_buffer_combined, raster_template, field = 1, fun=sum, background = 0)
bus_raster <- raster::rasterize(bus_buffer_merged, raster_template, field = 1, fun=sum, background = 0)
train_raster <- raster::rasterize(train_buffer_merged, raster_template, field = 1, fun = sum, background = 0)

# Convert all census tract level data to rasters
transpo_raster <- raster::rasterize(final_tracts_scaled, raster_template, field = final_tracts_scaled$transpo_cost, fun=sum)
job_access_raster <- raster::rasterize(final_tracts_scaled, raster_template, field = final_tracts_scaled$job_access, fun=sum)
envi_hazard_raster <- raster::rasterize(final_tracts_scaled, raster_template, field = final_tracts_scaled$envi_hazard, fun=sum)
econ_mobile_raster <- raster::rasterize(final_tracts_scaled, raster_template, field = final_tracts_scaled$prob_mean, fun=sum)

## Add all rasters together for 1 unitary score
final_raster <- grocery_raster + parks_raster + bus_raster + train_raster + transpo_raster + job_access_raster + envi_hazard_raster + econ_mobile_raster 


## Write raster images to file so we can access them without processing all of the data again
## 
writeRaster(grocery_raster, filename="grocerty_raster.grd", format="GTiff")
writeRaster(parks_raster, filename="parks_raster.grd", format="GTiff")
writeRaster(bus_raster, filename="bus_raster.grd", format="GTiff")
writeRaster(train_raster, filename="train_raster.grd", format="GTiff")
writeRaster(transpo_raster, filename="transpo_raster.grd", format="GTiff")
writeRaster(job_access_raster, filename="job_access_raster.grd", format="GTiff")
writeRaster(envi_hazard_raster, filename="envi_hazard_raster.grd", format="GTiff")
writeRaster(econ_mobile_raster, filename="econ_mobile_raster.grd", format="GTiff")
writeRaster(final_raster, filename="final_raster.grd", format="GTiff")


## Read in City of Atlanta property data


parcels <- st_read(paste(dir,"/Tax_Parcels_2018/Tax_Parcels_2018.shp",sep = '')) %>%
  st_transform(26967)  %>%
  select(
    OBJECTID,ParcelID, ClassCode, LivUnits, Address, Owner, TotAppr, LandAppr, ImprAppr, TotAssess, LandAssess, ImprAssess, LandAcres
  ) %>%
  mutate(
    OWNERNME1 = Owner
  ) 

parcels <- parcels[city,]

Atlanta_buildings <- st_read(paste(dir, "/Building_Footprints/Building_Footprints.shp", sep=""))%>%
  st_transform(26967)


## Attempt to sort property data by Municipal Ownership
atlanta_parcels_sort <- parcels %>%
  filter(grepl("atlanta", parcels$OWNERNME1, ignore.case = TRUE))
atlanta_parcels_sort_final <- atlanta_parcels_sort %>%
  filter(grepl("aecf", atlanta_parcels_sort$OWNERNME1, ignore.case = TRUE) |  grepl("authority", atlanta_parcels_sort$OWNERNME1, ignore.case = TRUE) | grepl("beltline", atlanta_parcels_sort$OWNERNME1, ignore.case = TRUE) | grepl("development", atlanta_parcels_sort$OWNERNME1, ignore.case = TRUE) | grepl("education", atlanta_parcels_sort$OWNERNME1, ignore.case = TRUE))

andp_parcels <- parcels %>%
  filter(grepl("andp", parcels$OWNERNME1, ignore.case = TRUE))
andp_parcels_final <- andp_parcels %>%
  filter(grepl("RESTORATION", andp_parcels$OWNERNME1, ignore.case = TRUE) | grepl("home", andp_parcels$OWNERNME1, ignore.case = TRUE))

development_parcels <- parcels %>%
  filter(grepl("development", parcels$OWNERNME1, ignore.case = TRUE) & grepl("community", parcels$OWNERNME1, ignore.case = TRUE))
development_parcels_final <- development_parcels %>%
  filter(grepl("butler", development_parcels$OWNERNME1, ignore.case = TRUE)|grepl("cabbage", development_parcels$OWNERNME1, ignore.case = TRUE)|grepl("investment", development_parcels$OWNERNME1, ignore.case = TRUE)|grepl("english", development_parcels$OWNERNME1, ignore.case = TRUE)|grepl("limits", development_parcels$OWNERNME1, ignore.case = TRUE)|grepl("summech", development_parcels$OWNERNME1, ignore.case = TRUE)|grepl("university community", development_parcels$OWNERNME1, ignore.case = TRUE))

fulton_parcels <- parcels %>%
  filter(grepl("fulton", parcels$OWNERNME1, ignore.case=TRUE))
fulton_parcels_final <- fulton_parcels %>%
  filter(grepl("county", fulton_parcels$OWNERNME1, ignore.case=TRUE)|grepl("land", fulton_parcels$OWNERNME1, ignore.case=TRUE))
fulton_parcels_final2 <- fulton_parcels_final %>%
  filter(!grepl("brinson", fulton_parcels_final$OWNERNME1, ignore.case=TRUE)&!grepl("mac holdings", fulton_parcels_final$OWNERNME1, ignore.case=TRUE)&!grepl("ACQUISITION", fulton_parcels_final$OWNERNME1, ignore.case=TRUE))

habitat_parcels <- parcels %>%
  filter(grepl("habitat", parcels$OWNERNME1, ignore.case=TRUE) & !grepl("urban", parcels$OWNERNME1, ignore.case = TRUE))

marta_parcels <- parcels %>%
  filter(parcels$OWNERNME1 == "MARTA")

urban_parcels <- parcels %>%
  filter(grepl("urban", parcels$OWNERNME1, ignore.case=TRUE) & grepl("residential", parcels$OWNERNME1, ignore.case = TRUE) & grepl("finance auth", parcels$OWNERNME1, ignore.case = TRUE))

municipal_parcels <- rbind(atlanta_parcels_sort_final, andp_parcels_final) %>%
  rbind(., development_parcels_final) %>%
  rbind(., fulton_parcels_final2) %>%
  rbind(., habitat_parcels) %>%
  rbind(., marta_parcels) %>%
  rbind(., urban_parcels) %>%
  mutate(
    improvedRatio = LandAppr/ TotAppr 
  )


## Remove any land that has living units on it unless the living units are fewer than allowed by zone
## Filter by any parcel that has a improved value to land value of less than 1 == doesn't really work
## a lot of the city parcels have improvements but they arent assessed.

## read in polygon file that has buildings. clip by parcels that do not have buildings
## Subset the parcels by which parcels have buildings

inside <- municipal_parcels[Atlanta_buildings, ]
sel <- over(as(municipal_parcels, "Spatial"), as(Atlanta_buildings, "Spatial"))
outside <- municipal_parcels[is.na(sel), ]

municipal_vacant_parcels <- outside %>%
  drop_na()

## Subset buildings by municipal parcels and then do the opposite

## Subset buildings by the city boudary
## subset the buildings by the municipally owned parcels
buildings <- Atlanta_buildings[city, ]
buildings <- buildings[municipal_parcels, ]

## filter municipal parcels that their building value is equal to or less than total parcel value and 
## that are worth more than $100 and less than $1,000,000
## filtering out the $100 valuations cleans the parcels that have been clearly not been assessed
## filtering out the <$1,000,000 assessments eliminates parcels that still clearly have a building on them
municipal_under_utilized_parcels <- municipal_parcels %>%
  filter(municipal_parcels$improvedRatio <= 1.0 & municipal_parcels$ImprAppr > 100 & municipal_parcels$ImprAppr < 1000000)

View(municipal_parcels)

st_write(municipal_under_utilized_parcels, "municipal_under_utilized_parcels.shp")
st_write(municipal_vacant_parcels, "municipal_vacant_parcels.shp")
st_write(municipal_parcels, "municipal_parcels.shp")


# Overlay the raster data to each municipally owned property
property_ranks_final <- raster::extract(x=final_raster,y= municipal_parcels, fun=mean,df=TRUE)
property_ranks_econ <- raster::extract(x=econ_mobile_raster ,y= municipal_parcels, fun=mean,df=TRUE)
property_ranks_bus <- raster::extract(x=bus_raster,y= municipal_parcels, fun=mean,df=TRUE)
property_ranks_envi <- raster::extract(x=envi_hazard_raster,y= municipal_parcels, fun=mean,df=TRUE)
property_ranks_grocery <- raster::extract(x=grocery_raster,y= municipal_parcels, fun=mean,df=TRUE)
property_ranks_job <- raster::extract(x=job_access_raster ,y= municipal_parcels, fun=mean,df=TRUE)
property_ranks_parks <- raster::extract(x=parks_raster,y= municipal_parcels, fun=mean,df=TRUE)
property_ranks_train <- raster::extract(x=train_raster ,y= municipal_parcels, fun=mean,df=TRUE)
property_ranks_transpo <- raster::extract(x=transpo_raster ,y= municipal_parcels, fun=mean,df=TRUE)


# Add ID to raster overlay list to then join with the original Municipal parcels data
property_ranks_final$OBJECTID <- municipal_parcels$OBJECTID
property_ranks_econ$OBJECTID <- municipal_parcels$OBJECTID
property_ranks_bus$OBJECTID <- municipal_parcels$OBJECTID
property_ranks_envi$OBJECTID <- municipal_parcels$OBJECTID
property_ranks_grocery$OBJECTID <- municipal_parcels$OBJECTID
property_ranks_job$OBJECTID <- municipal_parcels$OBJECTID
property_ranks_parks$OBJECTID <- municipal_parcels$OBJECTID
property_ranks_train$OBJECTID <- municipal_parcels$OBJECTID
property_ranks_transpo$OBJECTID <- municipal_parcels$OBJECTID


property_ranks <- property_ranks_final %>%
  left_join(.,property_ranks_econ, by="OBJECTID")%>%
  left_join(.,property_ranks_bus, by="OBJECTID")%>%
  left_join(.,property_ranks_envi, by="OBJECTID")%>%
  left_join(.,property_ranks_grocery, by="OBJECTID")%>%
  left_join(.,property_ranks_job, by="OBJECTID")%>%
  left_join(.,property_ranks_parks, by="OBJECTID")%>%
  left_join(.,property_ranks_train, by="OBJECTID")%>%
  left_join(.,property_ranks_transpo, by="OBJECTID") %>%
  mutate(
    ID = ID.x,
    OBJECTID = OBJECTID,
    final_score = layer.x,
    econ_score = layer.y,
    bus_score= layer.x.x ,
    envi_score = layer.y.y,
    grocery_score = layer.x.x.x,
    job_score = layer.y.y.y, 
    parks_score = layer.x.x.x.x, 
    train_score = layer.y.y.y.y,
    transpo_score = layer
  ) %>%
  select(
    ID, OBJECTID, final_score, econ_score, bus_score, envi_score, grocery_score, job_score, parks_score,train_score, transpo_score
  )


# Join data for final scores for each parcel sort 
# add an indicator variable to show which sort it fits with

municipal_under_utilized_parcels_ranked <- inner_join(municipal_under_utilized_parcels, property_ranks, by="OBJECTID") %>%
  mutate(
    under_utilized = TRUE
  ) %>%
  unique() %>%
  select(
    OBJECTID, 
    under_utilized
  )


municipal_vacant_parcels_ranked <- left_join(municipal_vacant_parcels, property_ranks, by="OBJECTID") %>%
  mutate(
    vacant = TRUE
  ) %>%
  unique() %>%
  select(
    OBJECTID,
    vacant
  )

municipal_parcels_ranked <- left_join(municipal_parcels, property_ranks, by="OBJECTID")  %>%
  mutate(
    municipally_owned = TRUE
  )%>%
  unique()%>%
  select(
    OBJECTID,
    municipally_owned
  )

municipal_parcels_scores <- left_join(municipal_parcels, property_ranks, by="OBJECTID")  %>%
  unique()%>%
  select(
    OBJECTID, 
    final_score, 
    econ_score, 
    bus_score, 
    envi_score, 
    grocery_score, 
    job_score, 
    parks_score,
    train_score, 
    transpo_score
  )



st_geometry(municipal_under_utilized_parcels_ranked) <- NULL
st_geometry(municipal_vacant_parcels_ranked) <- NULL
st_geometry(municipal_parcels_ranked) <- NULL
st_geometry(municipal_parcels_scores) <- NULL

municipal_parcels_final <- municipal_parcels_ranked %>%
  left_join(., municipal_vacant_parcels_ranked, by="OBJECTID") %>%
  left_join(., municipal_under_utilized_parcels_ranked, by="OBJECTID") %>%
  left_join(., municipal_parcels_scores, by="OBJECTID")
 

municipal_parcels_final <- inner_join(municipal_parcels, municipal_parcels_final, by= c("OBJECTID"))


View(municipal_parcels_final)

st_write(municipal_parcels_final, "municipal_parcels_final.shp")



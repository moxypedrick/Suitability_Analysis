# Identifying Municipally Owned Land Suitable for Affordable Housing Development

Affordable housing suitability analysis using R

-Identify unused municipally owned land for affordable housing development
-Create rubric to measure suitableness for affordable housing
-Analyze and rank each available property to prioritize and incent development

 -8 Factor Suitability Analysis
  -Created Raster Image for each layer
  -Added all images together for 1 score
  -Perfect score is 8
-Fulton County Tax Parcel Data
  -Filtered parcel data for just municipally owned parcels
  -Used OpenRefine to combine names that are similar
-Determine which municipally owned parcels are vacant or underutilized
  -Underutilized = Improved value / Total Value  <= 1.0 & Improved value  > 100 &  < 1000000
  -Vacant = overlayed a building shapefile and any parcel that didn’t have a building
Raster Extract raster values to tax parcel polygons


The Final Project.R file completes all data processing. The files that it produces are then moved to the 
final_data_files folder and used in the Final_Images.R script. This script creates all of the visualizations
and the html file which is hosted here: https://moxypedrick.github.io/Suitability_Analysis/. In the Final_Images.R 
script there is a step which exports files to OpenRefine. This step is used to combine common ownership
entities which have slightly different names (e.g., Housing Authority of City of Atlanta and City of Atlanta Housing
Authority). 





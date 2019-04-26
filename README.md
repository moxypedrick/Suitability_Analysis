# Identifying Municipally Owned Land Suitable for Affordable Housing Development

These R scripts and resulting <a href="https://moxypedrick.github.io/Suitability_Analysis/">map</a> are an analysis of municipally owned and community land resources which could be leveraged for the development of affordable housing in the City of Atlanta. 

The R scripts to complete the analysis are included as well as the R script to make our visualizations. The data used in this analysis can be found <a href="https://www.dropbox.com/s/e7crb2nb4vde9y7/Data_files.zip?dl=0">here</a> or downloaded individually from the following resources: 
<ul>
  <li><a href="http://arc-garc.opendata.arcgis.com/datasets/HUD::low-transportation-cost-index">Transportation Cost</a></li>
  <li><a href="http://arc-garc.opendata.arcgis.com/datasets/HUD::environmental-health-hazard-index">Environmental Hazards</a></li>
  <li><a href="https://opportunityinsights.org/data/">Economic Mobility</a></li>
  <li><a href="https://opportunityinsights.org/data/">Grocery Data</a></li>
  <li><a href="https://www.cnt.org/tools/housing-and-transportation-affordability-index">Job Access</a></li>
  <li><a href="https://dcp-coaplangis.opendata.arcgis.com/datasets/parks-1">Park Proximity</a></li>
  <li><a href="http://arc-garc.opendata.arcgis.com/datasets/COSS::marta-stops">Bus Stop Proximity</a></li>
  <li><a href="http://arc-garc.opendata.arcgis.com/datasets/COSS::marta-stops">Train Station Proximity</a></li>
  <li><a href="http://gisdata.fultoncountyga.gov/datasets/tax-parcels-2018?geometry=-292.324%2C-52.268%2C292.324%2C52.268">Tax Parcels</a></li>
  <li><a href="http://gisdata.fultoncountyga.gov/datasets/coaplangis::building-footprints">Building Footprints</a></li>
</ul>

###### Identify which parcels in the City of Atlanta are owned by municipal organizations or community organizations
We aquired a list of ownership entities which are municipal organizations or community organizations. With that list of owners, we completed filtering of all tax parcels. This was completed using the grepl function to allow us to search for combinations of text strings. The tax parcel data is very messy so the filter function needed flexibility. Once this was complete, we used OpenRefine (should have done this initially) to clean and cluster the owners. 

###### Create the 8 Factor Suitability Map
We used 8 data sources to create a suitability score for each parcel in the City of Atlanta. Each data source was converted to a Raster Image and all Raster Images were added together. 

###### Extract Raster Values to tax parcels
Next we extracted the raster values to each identified tax parcel. 



# Code to load and process PPR data after geocoding.
# 16th June 2017

library(data.table)
library(ggplot2)
library(stringr)
library(maptools)
library(sp)
library(lubridate)
# First load all data into R
ppr_data = list()
geocoded_data = list()
for (year in c(2012, 2013, 2014, 2015, 2016, 2017)){
  geocoded_data[[year]] = fread(paste0('data/output-', year, '.csv'))
  geocoded_data[[year]]$year = year
  ppr_data[[year]] = fread(paste0('data/PPR-', year, '.csv'))
  ppr_data[[year]]$year = year
}
# combine each of the dataframes in the list into one large dataframe
ppr_data <- do.call(rbind, ppr_data)
geocoded_data <- do.call(rbind, geocoded_data)

# Some data cleansing - prices need to be fixed to be integers
names(ppr_data) <- str_replace_all(tolower(names(ppr_data)), " ", "_") 
names(ppr_data)[5] <- "price"
ppr_data$price <- as.numeric(str_replace_all(ppr_data$price, "€|,", ""))
names(ppr_data)[4] <- 'ppr_county'
names(ppr_data)[1] <- "sale_date"
ppr_data$date <- lubridate::dmy(ppr_data$sale_date)

# Now combine the geocoded data with tte non-geocoded data (based on address and year)
# To combine these we need to "recreate" the "formatted string" in geocoded data.
ppr_data[, input_string:=paste(address, ppr_county, "Ireland", sep = ',')]
ppr_data <- merge(ppr_data, 
                  geocoded_data[, list(formatted_address, accuracy, latitude, longitude, 
                                       postcode, type, year, input_string)],
                  by=c("year", "input_string"), 
                  all.x = TRUE, allow.cartesian = FALSE)

# Now overlay the small areas from the census data
# load small area files - remember this needs to be in GPS form for matching.
map_data <- readShapePoly('Census2011_Small_Areas_generalised20m/small_areas_gps.shp')

# Assign a small area and electoral district to each property with a GPS coordinate.
# The assignment of points to polygons is done using the sp::over() function.
# Inputs are a SpatialPoints (house locations) set, and SpatialPolygons (boundary shapes)
spatial_points <- SpatialPointsDataFrame(coords = ppr_data[!is.na(latitude),.(longitude,latitude)], data=ppr_data[!is.na(latitude), .(input_string, postcode)])
polygon_overlap <- over(spatial_points, map_data)

# Now we can merge the Small Area / Electoral District IDs back onto the ppr_data.
ppr_data[!is.na(latitude), geo_county:=polygon_overlap$COUNTYNAME]
ppr_data$geo_county = str_replace(ppr_data$geo_county, pattern = " County", replacement = "")
ppr_data[!is.na(latitude), electoral_district:=polygon_overlap$EDNAME]
ppr_data[!is.na(latitude), electoral_district_id:=polygon_overlap$CSOED]
ppr_data[!is.na(latitude), region:=polygon_overlap$NUTS3NAME]
ppr_data[!is.na(latitude), small_area:=polygon_overlap$SMALL_AREA]

# Save output for analysis
write.csv(ppr_data, file = "ppr_data_encoded.csv")
ppr_data




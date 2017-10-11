library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(rgdal)
library(ggmap)
library(scales)
# Quick analysis of geocoded data - show some possibilities using dplr
data <- fread('./ppr_data_encoded.csv')
data$date <- data[, ymd(date)]

# ---------- Match rates and accuracies
# Overall match rate - where has Google found a match?
table(is.na(data$latitude))
table(is.na(data$latitude))/nrow(data)
# 6.6 % addresses are not matched. 93.4% match rate.

# Accuracy types
data[is.na(latitude), accuracy := 'NO_MATCH']
data %>%
  ggplot(aes(x=accuracy)) + geom_bar() + 
  scale_y_continuous(labels=comma) + labs(x="Google Accuracy", y="Number of entries")

table(data$accuracy)
table(data$accuracy)/nrow(data) * 100
ggplot(data, aes(x=accuracy)) + geom_bar()
# Approxy 60 % rooftop accuracy, and 20.1% approximate (what is approximate to google?)
# Geometric centre and range interpolated is less accurate.

# There are a few houses that Google matched to non-irish addresses, or where the spatial polygons
# from the census don't match the Google GPS co-ordinates. Assess and remove:
# For these, we have coordinates, but no match for the Electoral districts!
# Missed about 395 houses that didn't Geocode to an address in Ireland.
nrow(data[!is.na(latitude) & is.na(electoral_district)])

# world map showing missed addresses
ggmap(get_map('Ireland', zoom=1), extent = 'panel') + 
  geom_point(data=data[!is.na(latitude) & is.na(electoral_district), .(latitude, longitude)], 
             aes(y=latitude, x=longitude)) + 
  ylim(-50,70)

# Show errors in Ireland - these occur on coastal and border areas.
ggmap(get_map('Ireland', zoom=7), extent = 'panel') + 
  geom_point(data=data[!is.na(latitude) & is.na(electoral_district), .(latitude, longitude)], 
             aes(y=latitude, x=longitude))

# Lets see number per county overall, and number of misses per county.
ggplot(data[is.na(latitude)], aes(x=reorder(ppr_county, ppr_county, function(x)-length(x)))) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="County", y="Houses", title="Geocode Matches per County")

ggplot(data, aes(x=reorder(geo_county,geo_county,function(x)-length(x)))) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(x="County", y="Houses", title="Geocode County Results")

# remove these places that didn't get an electoral district / didn't get geocoded.
data <- data[!is.na(electoral_district)]

# How is our match on counties
# Check for error rates at a county level:
data[, city_house := geo_county %in% c("Dublin City", "South Dublin", "Dn Laoghaire-Rathdown", "South Dublin", "Fingal",
                                 "Galway City", "Cork City", "Limerick City", "Waterford City")]
data[, county_check := geo_county]
data[county_check %in% c('Dublin City', "Fingal", "Dn Laoghaire-Rathdown", "South Dublin"), county_check := "Dublin"]
data[county_check %in% c('Galway City'), county_check := "Galway"]
data[county_check %in% c('Cork City'), county_check := "Cork"]
data[county_check %in% c('Limerick City'), county_check := "Limerick"]
data[county_check %in% c('Waterford City'), county_check := "Waterford"]
data[county_check %in% c('South Tipperary', 'North Tipperary'), county_check := "Tipperary"]
table(data[!is.na(latitude), ppr_county], data[!is.na(latitude), county_check])

data[, city := geo_county %in% c("Dublin City", "South Dublin", "Dn Laoghaire-Rathdown", "South Dublin", "Fingal", "Galway City", "Cork City", "Limerick City", "Waterford City")]

# Spot any massive issues with a heatmap:
heatmap(as.matrix(table(data[!is.na(latitude), ppr_county], data[!is.na(latitude), county_check])), Colv=NA, Rowv=NA)

# How many errors do we have:
data %>% 
  filter(county_check != ppr_county) %>% 
  group_by(ppr_county) %>%
  ggplot(aes(x=ppr_county)) + geom_bar() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x="PPR County", y="Houses", title="County mis-matches Matches per County")

# Number of per month, with different areas?
library(lubridate)
ggplot(data[, .N, .(month=floor_date(date, 'month'))][order(month)], 
       aes(x=month, y=N)) + 
  geom_line() + 
  geom_smooth() + 
  geom_point() + 
  scale_y_continuous(labels = comma) +
  labs(x="Month", y="Number of Sales", title="House sales per month")

# Plotting of Median sale prie
ggplot(data[, .(price=median(price)), .(month=floor_date(date, 'month'))][order(month)], 
       aes(x=month, y=price)) + 
  geom_line() + 
  geom_smooth() + 
  geom_point() + 
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = 'year', date_labels = "%Y") +
  labs(x="Month", y="Median Sale Price", title="Median sale prices countrywide")

# Split median sale price by city and county sales
ggplot(data[, .(price=median(price)), .(city, month=floor_date(date, 'month'))][order(month)], 
       aes(x=month, y=price, col=city)) + 
  geom_line() + 
  geom_smooth() + 
  geom_point() + 
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = 'year', date_labels = "%Y") +
  labs(x="Month", y="Median Sale Price", title="Median sale prices city vs country")

# Examination of individual city sales
ggplot(data[city_house==TRUE, 
            .(.N, 
              price=median(price)), 
            .(county_check, month=floor_date(date, 'quarter'))][order(month)], 
       aes(x=month, y=price, col=county_check)) + 
  geom_line() + 
  geom_point() + 
  labs(x="Month", y="Median Sale Price (€)", title="Median sale prices per city") +
  scale_y_continuous(labels=comma) + 
  scale_x_date(date_breaks = 'year', date_labels = "%Y")

# Place with most sales, least sales.
ggplot(data[, .(.N), .(electoral_district, ppr_county)][order(-N)][1:10], 
       aes(x=electoral_district, y=N, fill=ppr_county)) +
  geom_bar(stat='identity') +
  labs(x="Electoral District", y="Number of house sales", 
       title="Electoral districts with most sales", fill="County") +
  theme(axis.text.x = element_text(angle=45, hjust=1))
  
# Most expensive electoral district
ed_with_25_sales <- data[, .N, electoral_district][N > 30]
ggplot(data[electoral_district %in% ed_with_25_sales$electoral_district, 
            .(price=mean(price)), 
            .(electoral_district, ppr_county)][order(-price)][1:10], 
       aes(x=electoral_district, y=price, fill=ppr_county)) +
  geom_bar(stat='identity') +
  labs(x="Electoral District", y="Mean House Sale Price (€)", 
       title="Most expensive electoral districts", fill="County") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_y_continuous(labels=comma)

# Map of Dublin with the ED lines and the points for sales
# you have to "fortify" the SHP polygons for this.

# Filter the map data to get Dublin:
shp <- readOGR('Census2011_Electoral_Divisions_generalised20m/electoral_divisions_gps.shp')
# we only want the shapes for the dublin area for this example
dublin_counties <- c("Fingal", "Dn Laoghaire-Rathdown", "Dublin City", 
                     "South Dublin", "Kildare County", "Wicklow County")
subset <- shp[as.character(shp@data$COUNTYNAME) %in% 
                dublin_counties, ]
mapdata <- fortify(subset)

data[, capped_price := price]
data[price > 1000000, capped_price := 1000000]
ggmap(get_map('Dublin, Ireland', zoom=12), extent = 'device') + 
  geom_polygon(data=mapdata, aes(x=long, y=lat, group=group), fill='grey', alpha=0.4, color='white') +
  geom_point(data=data[geo_county %in% dublin_counties & year==2017 & !is.na(latitude)], 
             aes(x=longitude, y=latitude, colour=capped_price)) +
  scale_color_continuous(label=comma)


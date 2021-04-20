library(pacman)

p_load("tidyverse", "httr", "jsonlite", "janitor", "usmap", "lubridate", "shiny", "hrbrthemes", 
       "viridis", "shinyWidgets", "leaflet", "sf", "maps")

# This project is the code for practicing shiny apps. I want to take the counties of CO
# and provide summary housing statistics for each county.

# fips codes for CO counties -------------------------------------------------------------------------

res_fips <- GET("https://data.colorado.gov/resource/gqse-2qk8.json")

data_fips <- fromJSON(rawToChar(res_fips$content))

data_fips_2 <- data_fips %>% 
  select(fips_code, fips_state_and_county_code)

# Data cleaning----------------------------------------------------------------------------------------


# Reading in permit data for housing
permit_data <- read_csv("C:/Users/Jake/Desktop/r_projects/shiny_projects/co_housing_vacancies/building_permit_counts_in_colorado.csv")


# Selecting data at the county level rather than include all cities
permit_data_2 <- permit_data %>% 
  filter(placeFips == "00000") %>% 
  select(-c("placeFips", "sdoBuildingPermit"))

# Adding in a column that describes the % of vacant houses by county
permit_data_3 <- permit_data_2 %>% 
  mutate(vacancy_percentage = (vacantHousingUnits/totalHousingUnits)*100) %>% 
  mutate(vacancy_percentage = format(round(vacancy_percentage, 2), nsmall = 2))

# Getting and attaching 5 digit county fips code for mapping onto usmaps package
permit_data_4 <- left_join(permit_data_3, data_fips_2, by = c("countyFips" = "fips_code")) %>% 
  na.omit()


# Selecting relevant data from previous dataframe, renaming fips to work with usmaps package
permit_data_5 <- permit_data_4 %>% 
  select(fips_state_and_county_code , area, year, totalPopulation, totalHousingUnits, vacantHousingUnits, vacancy_percentage, personsPerHousehold) %>% 
  rename("fips" = "fips_state_and_county_code") %>% 
  mutate(vacancy_percentage = as.numeric(vacancy_percentage))

# Map tests-----------------------------------------------------------------------------------------
# Trying to make leaflet work with maps package and data

permit_data_test <- permit_data_5 %>% 
  filter(year == 2015)


# checking to see what map looks like 
co_map <- st_as_sf(map("county", "colorado", plot = FALSE, fill = TRUE))

# getting fips and location information
county_fips <- county.fips %>% 
  mutate(fips = paste0("0", fips))

# Merging fips data and name data
co_map_2 <- merge(permit_data_test, county_fips)

# Merging coordinate data
co_map_3 <- merge(co_map_2, co_map, by.x = "polyname", by.y = "ID")

# Setting a color pallette for leaflet 
col_pal <- colorQuantile(palette = "viridis", domain = co_map_3$vacancy_percentage, n = 10)

#convert to sf
co_map_4 <- st_as_sf(co_map_3)
# Building a map
# Warning : Legend percentile is in relation to other CO counties, not true vacancy rate. 
# Remember to make a very very explicit note of this when publishing
co_map_4 %>% 
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(
    popup = ~paste0(area, "<br>", "Vacancy %: ", vacancy_percentage),
    stroke = FALSE,
    smoothFactor = 0,
    fillOpacity = 0.7,
    color = ~col_pal(vacancy_percentage)
  ) %>% 
  addLegend(
    "bottomright", 
    pal = col_pal, 
    values = ~vacancy_percentage,
    title = "Vacancy percentiles",
    opacity = 1
  )

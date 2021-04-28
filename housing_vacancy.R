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

# Map test-----------------------------------------------------------------------------------------
# Trying to make leaflet work with maps package and data

permit_data_test <- permit_data_5 %>% 
  filter(year == 2015)


# checking to see what map looks like 
co_map <- st_as_sf(map("county", "colorado", plot = FALSE, fill = TRUE))

# getting fips and location information, adding a 0 to fips column for merge
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
    title = "Percentile of CO counties",
    opacity = 1
  )
# Great success!!!!

# Leaflet prep ---------------------------------------------------------------------------------------
# Attaching columns for leaflet use
permit_data_6 <- merge(permit_data_5, county_fips)

permit_data_7 <- merge(permit_data_6, co_map, by.x = "polyname", by.y = "ID")

#cleaning names to look pretty for drop down menu
permit_data_8 <- clean_names(permit_data_7)
  
# Filtering housing vacancy percentages

permit_data_9 <- permit_data_8 %>% 
  select(polyname, geom, fips, area, year, vacancy_percentage)

# Assigning sf designation to data

permit_data_final <- st_as_sf(permit_data_9)

# Building a Shiny Ap --------------------------------------------------------------------------------



ui <- fluidPage(
  navbarPage(title = "Colorado Housing Data",
    tabPanel(title = "County Comparison",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "year",
            label = "Year",
            choices = sort(permit_data_final$year)
          )
          # br(),
          # 
          # selectInput(
          #   inputId = "sel_data",
          #   label = "Data to Display",
          #   choices = c("Total Population" = "total_population",
          #               "Persons Per Household" = "persons_per_household",
          #               "Total Housing Units" = "total_housing_units",
          #               "Vacant Housing Units" = "vacant_housing_units",
          #               "Vacancy Percentage" = "vacancy_percentage"
          #   ),
          #   multiple = FALSE
          #   # uiOutput("total_population"),
          #   # uiOutput("persons_per_household"),
          #   # uiOutput("total_housing_units"),
          #   # uiOutput("vacant_housing_units"),
          #   # uiOutput("vacancy_percentage")
          # )
          ),
        mainPanel(
          leafletOutput("heat_map"),
          plotOutput("bar_graph") 
        ))
      ),
      tabPanel(title = "Counties over Time",
               sidebarLayout(
                 sidebarPanel(
                   inputPanel(
                     selectInput(
                       inputId = "counties",
                       label = "Select Counties",
                       choices = sort(unique(permit_data_final$area)),
                       multiple = TRUE
                     )),
                     # br(),
                     # selectInput(
                     #   inputId = "sel_data_2",
                     #   label = "Data to Display",
                     #   choices = c("Total Population" = "total_population",
                     #               "Persons Per Household" = "persons_per_household",
                     #               "Total Housing Units" = "total_housing_units",
                     #               "Vacant Housing Units" = "vacant_housing_units",
                     #               "Vacancy Percentage" = "vacancy_percentage"
                     #   ),
                     #   multiple = FALSE,

                 ),
                 mainPanel(
                   plotOutput("line_graph")
                 )))))


  


server <- function(input, output, session) {
  
  county_leaflet_map <- reactive({
    req(input$year)
    filter(permit_data_final, year %in% input$year)
  })
  
  output$heat_map <- renderLeaflet({
    county_leaflet_map() %>% 
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
        title = "Percentile of CO counties",
        opacity = 1
      )
      
    
    
  })
  
  county_line_graph <- reactive({
    req(input$counties)
    filter(permit_data_final, area %in% input$counties)
  })
  
  output$line_graph <- renderPlot({
    input$counties
      ggplot(data = county_line_graph()) +
        geom_line(mapping = aes(x = year, y = vacancy_percentage , group = area, color = area), lwd = 1.5) +
        labs(title = "Vacancy % by County 2010 - 2018", color = "County") +
        xlab("Year") +
        ylab("Percent Vacant") +
        scale_color_viridis(discrete = TRUE) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
  })

  # county_comparison <- reactive({
  #   req(input$year)
  #   req(input$sel_data)
  #   filter(permit_data_final, year %in% input$year) %>% 
  #     select(permit_data_final$polyname, permit_data_final$geom, permit_data_final$fips, 
  #            permit_data_final$area,
  #            permit_data_final$year, 
  #            get(input$sel_data))
  #   
  # })
  # 
  # county_line_graph <- reactive({
  #   req(input$counties)
  #   req(input$sel_data_2)
  #   filter(permit_data_final, area %in% input$counties) %>% 
  #     select(permit_data_final$area,
  #            permit_data_final$year, 
  #            get(input$sel_data_2))
  # 
  # })
  # 
  # output$line_graph <- renderPlot({
  #   input$counties
  #   input$sel_data_2
  #   ggplot(data = county_line_graph()) +
  #     geom_line(mapping = aes(x = year, y = get(input$sel_data_2), group = area, color = area), lwd = 1.5) +
  #     labs(title = "Selected Data by County 2010 - 2018", color = "County") +
  #     xlab("Year") + 
  #     ylab("Percent Vacant") +
  #     scale_color_viridis(discrete = TRUE) +
  #     theme_minimal() +
  #     theme(plot.title = element_text(hjust = 0.5))
  # })

  
}



shinyApp(ui = ui, server = server)




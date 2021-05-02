library(pacman)

p_load("tidyverse", "httr", "jsonlite", "janitor", "usmap", "shiny", "hrbrthemes", 
       "viridis", "shinyWidgets", "leaflet", "sf", "maps", "scales", "shinydashboard",
       "shinythemes")

# This project is the code for practicing shiny apps. I want to take the counties of CO
# and provide visual statistics for each county. Major props to my former professors
# Ed Rubin and Grant Mcdermott for introducing me to R and visualizations. Please check out their work.

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

# Adding in a column that describes the % of vacant houses by county, making decimals pretty
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

# getting fips and location information, adding a 0 to fips column for merge
county_fips <- county.fips %>% 
  mutate(fips = paste0("0", fips))


# Leaflet prep ---------------------------------------------------------------------------------------
# Attaching columns for leaflet use
permit_data_6 <- merge(permit_data_5, county_fips)

permit_data_7 <- merge(permit_data_6, co_map, by.x = "polyname", by.y = "ID")

#cleaning names to look pretty for drop down menu
permit_data_8 <- clean_names(permit_data_7)
  
# Assigning sf designation to data

permit_data_final <- st_as_sf(permit_data_8)

# Building a Shiny Ap --------------------------------------------------------------------------------



ui <- fluidPage(
  theme = shinytheme("united"),
  navbarPage(title = "Colorado Housing Data",
    tabPanel(title = "County Comparison",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "year",
            label = "Year",
            choices = sort(permit_data_final$year)
          ),
          br(),

          selectizeInput(
            inputId = "sel_data",
            label = "Data to Display",
            choices = c("Total Population" = "total_population", # Relabeling columns so they look better in shiny
                        "Persons Per Household" = "persons_per_household",
                        "Total Housing Units" = "total_housing_units",
                        "Vacant Housing Units" = "vacant_housing_units",
                        "Vacancy Percentage" = "vacancy_percentage"
            ),
            multiple = FALSE
          )
          ),
        mainPanel(
          leafletOutput("heat_map"),
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
                     br(),
                     selectizeInput(
                       inputId = "sel_data_2",
                       label = "Data to Display",
                       choices = c("Total Population" = "total_population",
                                   "Persons Per Household" = "persons_per_household",
                                   "Total Housing Units" = "total_housing_units",
                                   "Vacant Housing Units" = "vacant_housing_units",
                                   "Vacancy Percentage" = "vacancy_percentage"
                       ),
                       multiple = FALSE)

                 ),
                 mainPanel(
                   plotOutput("line_graph")
                 )))))


  


server <- function(input, output, session) {
  
# For whatever reason not having a reactive function made leaflet work. According to many this is 
# bad practice but I haven't found a solution that works with a reactive function. I literally spent
# over 15 hours trying to make it work and it doesn't so I am going with this. 

  
  #leaflet map code
  
  output$heat_map <- renderLeaflet({
    input$year
    input$sel_data
    
    # Filtering data for leaflet map
    permit_leaflet_map <- permit_data_final %>% 
      select(area, year, geom, fips, polyname, input$sel_data) %>% 
      filter(year %in% input$year)
    
    # Setting the color palette. One of my former professors Ed Rubin is Red Green color blind and
    # recommended the viridis color palette so he could see it. This is for you Ed!
    col_pal_2 <- colorQuantile(palette = "viridis", domain = permit_leaflet_map[[input$sel_data]], n = 10)
    
    
    permit_leaflet_map %>% 
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet(width = "100%") %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(
        popup = ~paste0(area, "<br>", "Selected Data: ", get(input$sel_data)),
        stroke = FALSE,
        smoothFactor = 0,
        fillOpacity = 0.7,
        color = ~col_pal_2(get(input$sel_data))
      ) %>% 
      addLegend(
        "bottomright", 
        pal = col_pal_2, 
        values = ~get(input$sel_data),
        title = "Percentile in CO",
        opacity = 1
      )
      
    
    
  })
  
  #line graph code
  
  # Reactive function for line graph
  county_line_graph <- reactive({
    req(input$counties)
    req(input$sel_data_2)
    permit_data_final %>% 
      select(area, year, input$sel_data_2) %>% 
    filter(area %in% input$counties)
      
    
    
  })
  
  # Output for line graph
  output$line_graph <- renderPlot({
    input$counties
    input$sel_data_2
    

    
      ggplot(data = county_line_graph()) +
        geom_line(mapping = aes(x = year, y = get(input$sel_data_2) , group = area, color = area), lwd = 1.5) +
        labs(title = "Selected Data by County 2010 - 2018", color = "County") +
        xlab("Year") +
        ylab("Selected Data") +
        scale_y_continuous(labels = comma) +
        scale_color_viridis(discrete = TRUE) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5), axis.title.y = element_text(angle = 90, hjust = 0.5))
  })

}



shinyApp(ui = ui, server = server)




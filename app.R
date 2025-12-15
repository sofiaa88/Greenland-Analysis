
# Load packages
library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(osmextract)
library(DT)  

# Load data
df <- read_csv("~/Desktop/1. MA615/MA615 Final Project_Greenland/2. Data/greenland.csv")
greenland_geo <- st_read("~/Desktop/1. MA615/MA615 Final Project_Greenland/2. Data/gadm41_GRL_1.json")

greenland_osm <- oe_read(
  "~/Desktop/1. MA615/MA615 Final Project_Greenland/2. Data/greenland-latest.osm.pbf",
  layer = "points",
  extra_tags = c("aeroway", "tourism", "amenity", "shop")
)

airports <- greenland_osm %>%
  filter(aeroway == "aerodrome")

hotels <- greenland_osm %>%
  filter(tourism == "hotel")

# Data Cleaning
df <- df[,-4]

df$region <- str_replace(df$region, "Kommuneqarfik Sermersooq \xd8st",
                         "Kommuneqarfik Sermersooq")
df$region <- str_replace(df$region, fixed("Ilulissat (*)"), "Ilulissat")

df <- df |>
  rename("overnight_stays" = "Overnight stays")

avg_stay <- df %>% 
  filter(unit == "Average overnight stays per guest")

guests <- df %>% 
  filter(unit == "Number of guests")

num_guests <- df %>% 
  filter(unit == "Number of guests")

guests_per_region <- df %>% 
  group_by(region) %>% 
  summarise(total_guests = round(sum(overnight_stays)))

guests_per_region_yr <- num_guests %>% 
  group_by(region, time) %>% 
  summarise(total_guests = round(sum(overnight_stays)))

nationality <- df %>% 
  filter(unit == "Number of guests") %>%
  filter(nationality != "Unknown") %>%
  group_by(nationality, region, time) %>% 
  summarise(total_guests = round(sum(overnight_stays, na.rm = TRUE)), .groups = "drop")

# UI
ui <- dashboardPage(
  header = dashboardHeader(title = "Greenland Tourism Dashboard"),
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("home")),
      menuItem("Heatmap", tabName = "heatmap", icon = icon("map")),
      menuItem("Region", tabName = "region", icon = icon("map-pin")), 
      menuItem("Average Overnight Stays", tabName = "time", icon = icon("chart-line")),
      menuItem("Tourism by Nationality", tabName = "nationality", icon = icon("globe")),
      menuItem("Cleaning & Analysis", tabName = "analysis", icon = icon("file-alt")), 
      menuItem("Sources", tabName = "sources", icon = icon("book-open"))
    )
  ),
  
  body = dashboardBody(
    tabItems(
      
      # Tab 1 - Introduction
      tabItem(tabName = "intro",
              h2("Greenland Tourism Data Exploration"),
              p("Use the sidebar to explore different aspects of the data."),
              p("- Heatmap: Tourist distribution across regions and years"),
              p("- Region: Guests by region"),
              p("- Mean Overnight Stays per Visit"),
              p("- Tourism by Nationality: Number of tourists from each nationality"),
              p("- Cleaning & Analysis"),
              p("- Sources")
      ),
      
      # Tab 2 - Heatmap
      tabItem(tabName = "heatmap",
              h2("Tourist Map by Region and Year"),
              sliderInput("map_year", "Select Year:",
                          min = min(guests$time, na.rm = TRUE),
                          max = max(guests$time, na.rm = TRUE),
                          value = max(guests$time, na.rm = TRUE),
                          step = 1,
                          animate = TRUE),
              leafletOutput("heatmap_plot", height = "600px")
      ),
      
      # Tab 3 - Region analysis
      tabItem(tabName = "region", 
              h2("Guests by Region"),
              selectInput("region_select", "Choose Region:",
                          choices = unique(guests_per_region$region),
                          multiple = TRUE,
                          selected = unique(guests_per_region$region)),
              plotOutput("race_plot"),
              textOutput("race_top"),
              DT::DTOutput("race_table")
      ),
      
      # Tab 4 - Average Stay
      tabItem(tabName = "time",
              h2("Average Overnight Stays per Visit"),
              plotOutput("avg_trend_plot")
      ),
      
      # Tab 5 - Nationality Trends
      tabItem(tabName = "nationality",
              h2("Tourism Trends by Nationality and Region"),
              plotOutput("nationality_plot", height = "700px")
      ),
      
      # Tab 6 - Analysis Report
      tabItem(tabName = "analysis",
              h2("Analysis Report"),
              
              fluidRow(
                box(
                  width = 12,
                  title = "Key Findings",
                  status = "success",
                  solidHeader = TRUE,
                  
                  h3("Tourism Trends in Greenland (2020-2024)"),
                  
                  h4("Data Cleaning"),
                  p("There was minimal cleaning needed for this dataset. Two of the region names needed to be standardized since there were different versions of the same place. An additional step was taken to convert the name of a column just so that it matched the aesthetics of the other column names."),
                  
                  h4("Heatmap:"),
                  p("The heatmap shows the number of tourists who visited Greenland by region and year. Darker colors represent higher visitor numbers. The sliding scale allows the user to change the year being observed and examine changes from 2020 to 2024. The hover feature is also enabled to provide a more detailed breakdown of the number of visitors, along with the name of the region. The layers panel on the upper right allows the user to deselect regional boundaries or select additional layers to view points of interest, such as hotels and airports."),
                  
                  h4("Region:"),
                  p("The region tab uses a bar plot to visualize the which regions have the most tourists overall, a sum of all the years. Avannaata Kommunia and Kommune Qeqertalik take the lead; they are two regions combined together. The dataset observed these two regions in combination, whereas the other entries represent separate regions."),
                  
                  h4("Average Stay Duration:"),
                  p("From the trend graph in the average stay tab, the line shows a decline in the average number of overnight stays across Greenland. We don't have the data before 2020, but we can hypothesize that the decline may have been influenced by COVID-19, and the average stay duration has not recovered to pre-decline levels as of 2024. The graph shows a downward trend from approximately 3.2 nights in 2020 to around 2.7 nights in 2024, with a notable dip to about 2.9 nights in 2021, followed by a slight recovery in 2022, but the overall trajectory remains downward through 2024. This information could be valuable for tourism departments and agencies across Greenland in addressing these declines and developing strategies to increase tourism. "),
                  
                  h4("Tourism by Nationality"),
                  p("displays tourism trends by nationality across all regions from 2020 to 2024. Each graph represents a different country of origin, with colored lines showing visitor distribution across Greenland's regions. Denmark shows the highest overall visitor numbers, followed by Germany and Greenland. Most nationalities display a similar recovery pattern: low numbers in 2020, sharp increases in 2021-2022, and continued growth or stabilization through 2024. Across nearly all nationalities, Avannaata Kommunia and Kommune Qeqertalik (green line) and Kommuneqarfik Sermersooq Vest (yellow line) attract the most visitors, while Kommuneqarfik Sermersooq (pink line) consistently shows the lowest visitation. Denmark appears to have a more even distribution of tourists across regions compared to other nationalities, which show stronger concentration in the northwestern areas. The consistent pattern across all nationalities reinforces the regional disparities observed in earlier figures, with western regions dominating tourism regardless of visitor origin.")
                
                )
              )
      ),
      
      # Tab 7 - Sources
      tabItem(tabName = "sources",
              h2("Data Sources"),
              
              h3("Tourism Data"),
              p("• Greenland Tourism Statistics (2020-2024)"),
              p("• Source: Statbank Greenland"),
              p("• URL: ", tags$a(href = "https://bank.stat.gl/pxweb/en/",
                                  "https://bank.stat.gl/pxweb/en/",
                                  target = "_blank")),
              
              br(),
              
              h3("Geographic Data"),
              p("• GADM Database of Global Administrative Areas"),
              p("• Version: 4.1, Level 1 (Regional boundaries)"),
              p("• URL: ", tags$a(href = "https://gadm.org/download_country.html", 
                                  "https://gadm.org/download_country.html", 
                                  target = "_blank")),
              
              br(),
              
              h3("Points of Interest Data"),
              p("• Geofabrik Downloads"),
              p("• URL: ", tags$a(href = "https://download.geofabrik.de/north-america/greenland.html", 
                                  "https://download.geofabrik.de/north-america/greenland.html", 
                                  target = "_blank")),
              
              br(),
              
              h3("Course Information"),
              p("MA615 - Applied Statistical Modeling"),
              p("December 2025")
      )
    ) 
  ),
  
  skin = "green"
)

# Server
server <- function(input, output, session){
  
  output$heatmap_plot <- renderLeaflet({
    
    req(input$map_year)
    
    year_data <- guests_per_region_yr %>%
      filter(time == input$map_year)
    
    name_mapping <- data.frame(
      region = c("Avannaata Kommunia and Kommune Qeqertalik",
                 "Ilulissat",
                 "Kommune Kujalleq", 
                 "Kommuneqarfik Sermersooq",
                 "Qeqqata Kommunia"),
      NAME_1 = c("Qaasuitsup", "Qaasuitsup", "Kujalleq", "Sermersooq", "Qeqqata")
    )
    
    year_data <- year_data %>%
      left_join(name_mapping, by = "region") %>%
      group_by(NAME_1) %>%
      summarise(total_guests = sum(total_guests, na.rm = TRUE), .groups = "drop")
    
    map_data <- greenland_geo %>%
      left_join(year_data, by = "NAME_1")
    
    pal <- colorBin(
      palette = "YlOrRd",
      domain = map_data$total_guests,
      bins = 5,
      na.color = "#E0E0E0"
    )
    
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(lng1 = -73, lat1 = 59, lng2 = -11, lat2 = 83) %>%
      
      addPolygons(
        fillColor = ~pal(total_guests),
        fillOpacity = 0.7,
        color = "white",
        weight = 2,
        group = "Regions",
        highlight = highlightOptions(
          weight = 3,
          color = "#667",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = ~paste0(NAME_1, ": ", 
                        ifelse(is.na(total_guests), "No data", 
                               format(total_guests, big.mark = ","))),
        popup = ~paste0("<strong>", NAME_1, "</strong><br>",
                        "Guests: ", 
                        ifelse(is.na(total_guests), "No data",
                               format(total_guests, big.mark = ",")), 
                        "<br>Year: ", input$map_year)
      ) %>%
      
      addCircleMarkers(
        data = airports,
        radius = 5,
        color = "blue",
        fillColor = "lightblue",
        fillOpacity = 0.8,
        popup = ~paste0("<b>Airport:</b> ", name),
        group = "Airports"
      ) %>%
      
      addCircleMarkers(
        data = hotels,
        radius = 3,
        color = "green",
        fillColor = "lightgreen",
        fillOpacity = 0.8,
        popup = ~paste0("<b>Hotel:</b> ", name),
        group = "Hotels"
      ) %>%
      
      addLayersControl(
        overlayGroups = c("Regions", "Airports", "Hotels"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~total_guests,
        title = "Number of Guests",
        opacity = 0.7,
        na.label = "No data"
      ) %>% 
      
      addLegend(
        position = "bottomright",
        colors = c("blue", "green"),
        labels = c("● Airports", "● Hotels"),
        title = "Points of Interest",
        opacity = 1
      ) 
  })
  
  output$race_plot <- renderPlot({
    guests_per_region %>%
      filter(region %in% input$region_select) %>%
      ggplot(aes(x = reorder(region, total_guests), y = total_guests)) +
      geom_col(fill = "seagreen") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      labs(x = "Region", y = "Number of Guests") +
      coord_flip()
  })
  
  output$race_table <- DT::renderDT({
    guests_per_region %>%
      arrange(desc(total_guests))
  })
  
  output$avg_trend_plot <- renderPlot({
    avg_stay %>%
      group_by(time) %>%
      summarise(avg_stays = mean(overnight_stays, na.rm = TRUE)) %>%
      ggplot(aes(x = time, y = avg_stays)) +
      geom_line(color = "darkgreen", size = 1.5) +
      geom_point(color = "darkgreen", size = 3) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
      theme_minimal() +
      labs(x = "Year", y = "Mean Stay Duration") +
      theme(
        plot.title = element_text(size = 16, face = "bold")
      )
  })
  
  output$nationality_plot <- renderPlot({
    nationality %>%
      ggplot(aes(x = time, y = total_guests, color = region, group = region)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      facet_wrap(~ nationality, scales = "free_y", ncol = 3) +
      scale_y_continuous(labels = scales::comma) +
      scale_color_brewer(palette = "Set2") +
      theme_minimal() +
      labs(x = "Year", 
           y = "Total Guests",
           color = "Region") +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      guides(color = guide_legend(nrow = 2))
  })
  
}

# Run app
shinyApp(ui, server)
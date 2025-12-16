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
df <- read_csv("greenland.csv")
greenland_geo <- st_read("gadm41_GRL_1.json")
greenland_osm <- oe_read(
  "greenland-latest.osm.pbf",
  layer = "points",
  extra_tags = c("aeroway", "tourism", "amenity", "shop")
)

airports <- greenland_osm %>%
  filter(aeroway == "aerodrome")

hotels <- greenland_osm %>%
  filter(tourism == "hotel")

# Data Cleaning
df <- df[, -4]

df$region <- str_replace(
  df$region,
  "Kommuneqarfik Sermersooq \xd8st",
  "Kommuneqarfik Sermersooq"
)
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
  summarise(total_guests = round(sum(overnight_stays)), .groups = "drop")

guests_per_region_yr <- num_guests %>%
  group_by(region, time) %>%
  summarise(total_guests = round(sum(overnight_stays)), .groups = "drop")

nationality <- df %>%
  filter(unit == "Number of guests") %>%
  filter(nationality != "Unknown") %>%
  group_by(nationality, region, time) %>%
  summarise(total_guests = round(sum(overnight_stays, na.rm = TRUE)), .groups = "drop")

# UI
ui <- dashboardPage(
  skin = "green",
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
    # Put CSS in the document head (global)
    tags$head(
      tags$style(HTML("
        /* Overall */
        .content-wrapper, .right-side { background-color: #f6f7f9; }
        .main-header .logo { font-weight: 600; }
        .main-header .navbar { box-shadow: 0 1px 0 rgba(0,0,0,0.06); }
        .sidebar-menu > li > a { font-size: 14px; }
        .sidebar-menu .active > a { font-weight: 600; }

        /* Page typography */
        h2 { margin-top: 6px; margin-bottom: 12px; font-weight: 650; }
        h3 { margin-top: 4px; font-weight: 650; }
        h4 { margin-top: 14px; font-weight: 650; }

        /* Boxes to look more like cards */
        .box {
          border-top: 0;
          border-radius: 12px;
          box-shadow: 0 6px 18px rgba(16,24,40,0.06);
        }
        .box-header { border-top-left-radius: 12px; border-top-right-radius: 12px; }
        .box-body { padding: 16px; }

        /* DT styling */
        table.dataTable { border-radius: 10px; overflow: hidden; }

        /* Images */
        .hero-img {
          width: 100%;
          max-width: 1050px;
          border-radius: 14px;
          display: block;
          margin: 0 auto 18px;
          box-shadow: 0 10px 30px rgba(0,0,0,0.10);
        }
        .support-img {
          width: 100%;
          max-width: 950px;
          border-radius: 14px;
          display: block;
          margin: 14px auto 0;
          box-shadow: 0 10px 30px rgba(0,0,0,0.08);
        }

        /* Subtle section helper text */
        .muted { color: #667085; font-size: 13px; margin-top: -6px; }
      "))
    ),
    
    tabItems(
      # Tab 1 - Introduction
      tabItem(
        tabName = "intro",
        
        fluidRow(
          box(
            width = 12,
            title = "Greenland Tourism Data Exploration",
            status = "success",
            solidHeader = TRUE,
            
            # Image 1 (original)
            tags$img(
              tags$img(src = "Ilulissat_Icefjord.jpg", class = "hero-img")
            ),
            
            p(
              class = "muted",
              "Use the sidebar to explore different aspects of the data."
            ),
            
            tags$ul(
              tags$li("Heatmap: Tourist distribution across regions and years"),
              tags$li("Region: Guests by region"),
              tags$li("Mean Overnight Stays per Visit"),
              tags$li("Tourism by Nationality: Number of tourists from each nationality"),
              tags$li("Cleaning & Analysis"),
              tags$li("Sources")
            )
          )
        )
      ),
      
      # Tab 2 - Heatmap
      tabItem(
        tabName = "heatmap",
        
        fluidRow(
          box(
            width = 4,
            title = "Controls",
            status = "success",
            solidHeader = TRUE,
            p(class = "muted", "Slide through years to see changes in guests by region."),
            sliderInput(
              "map_year",
              "Select Year:",
              min = min(guests$time, na.rm = TRUE),
              max = max(guests$time, na.rm = TRUE),
              value = max(guests$time, na.rm = TRUE),
              step = 1,
              animate = TRUE
            )
          ),
          
          box(
            width = 8,
            title = "Tourist Map by Region and Year",
            status = "success",
            solidHeader = TRUE,
            leafletOutput("heatmap_plot", height = "640px")
          )
        )
      ),
      
      # Tab 3 - Region analysis
      tabItem(
        tabName = "region",
        
        fluidRow(
          box(
            width = 4,
            title = "Filters",
            status = "success",
            solidHeader = TRUE,
            p(class = "muted", "Select one or multiple regions to display."),
            selectInput(
              "region_select",
              "Choose Region:",
              choices = unique(guests_per_region$region),
              multiple = TRUE,
              selected = unique(guests_per_region$region)
            )
          ),
          
          box(
            width = 8,
            title = "Guests by Region",
            status = "success",
            solidHeader = TRUE,
            plotOutput("race_plot", height = "420px"),
            br(),
            textOutput("race_top")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Region Summary Table",
            status = "success",
            solidHeader = TRUE,
            DT::DTOutput("race_table")
          )
        )
      ),
      
      # Tab 4 - Average Stay
      tabItem(
        tabName = "time",
        
        fluidRow(
          box(
            width = 12,
            title = "Average Overnight Stays per Visit",
            status = "success",
            solidHeader = TRUE,
            plotOutput("avg_trend_plot", height = "420px"),
            
            # Image 2 (original)
            tags$img(tags$img(src = "houses.jpg", class = "support-img")
            )
          )
        )
      ),
      
      # Tab 5 - Nationality Trends
      tabItem(
        tabName = "nationality",
        
        fluidRow(
          box(
            width = 12,
            title = "Tourism Trends by Nationality and Region",
            status = "success",
            solidHeader = TRUE,
            p(class = "muted", "Faceted trends by nationality; scales vary by panel."),
            plotOutput("nationality_plot", height = "760px")
          )
        )
      ),
      
      # Tab 6 - Analysis Report (content unchanged)
      tabItem(
        tabName = "analysis",
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
            p("From the trend graph in the average stay tab, the line shows a decline in the average number of overnight stays across Greenland. We don't have the data before 2020, but we can hypothesize that the decline may have been influenced by COVID-19, and the average stay duration has not recovered to pre-decline levels as of 2024. The graph shows a downward trend from approximately 3.2 nights in 2020 to around 2.7 nights in 2024, with a notable dip to about 2.9 nights in 2021, followed by a slight recovery in 2022, but the overall trajectory remains downward through 2024. This information could be valuable for tourism departments and agencies across Greenland in addressing these declines and developing strategies to increase tourism."),
            
            h4("Tourism by Nationality"),
            p("From the multiple graphs, we can observe tourism trends by nationality across all regions from 2020 to 2024. Each graph represents a different country of origin, with colored lines showing visitor distribution across Greenland's regions. Denmark shows the highest overall visitor numbers, followed by Germany and Greenland. Most nationalities display a similar recovery pattern: low numbers in 2020, sharp increases in 2021-2022, and continued growth or stabilization through 2024. Across nearly all nationalities, Avannaata Kommunia and Kommune Qeqertalik (green line) and Kommuneqarfik Sermersooq Vest (yellow line) attract the most visitors, while Kommuneqarfik Sermersooq (pink line) consistently shows the lowest visitation. Denmark appears to have a more even distribution of tourists across regions compared to other nationalities, which show stronger concentration in the northwestern areas. The consistent pattern across all nationalities reinforces the regional disparities observed in earlier figures, with western regions dominating tourism regardless of visitor origin.")
          )
        )
      ),
      
      # Tab 7 - Sources (content unchanged)
      tabItem(
        tabName = "sources",
        h2("Data Sources"),
        
        fluidRow(
          box(
            width = 12,
            title = "Sources",
            status = "success",
            solidHeader = TRUE,
            
            h3("Tourism Data"),
            p("• Greenland Tourism Statistics (2020-2024)"),
            p("• Source: Statbank Greenland"),
            p("• URL: ", tags$a(
              href = "https://bank.stat.gl/pxweb/en/",
              "https://bank.stat.gl/pxweb/en/",
              target = "_blank"
            )),
            
            br(),
            
            h3("Geographic Data"),
            p("• GADM Database of Global Administrative Areas"),
            p("• Version: 4.1, Level 1 (Regional boundaries)"),
            p("• URL: ", tags$a(
              href = "https://gadm.org/download_country.html",
              "https://gadm.org/download_country.html",
              target = "_blank"
            )),
            
            br(),
            
            h3("Points of Interest Data"),
            p("• Geofabrik Downloads"),
            p("• URL: ", tags$a(
              href = "https://download.geofabrik.de/north-america/greenland.html",
              "https://download.geofabrik.de/north-america/greenland.html",
              target = "_blank"
            )),
            
            br(),
            
            h3("Course Information"),
            p("MA615 - Applied Statistical Modeling"),
            p("December 2025")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  output$heatmap_plot <- renderLeaflet({
    req(input$map_year)
    
    year_data <- guests_per_region_yr %>%
      filter(time == input$map_year)
    
    name_mapping <- data.frame(
      region = c(
        "Avannaata Kommunia and Kommune Qeqertalik",
        "Ilulissat",
        "Kommune Kujalleq",
        "Kommuneqarfik Sermersooq",
        "Qeqqata Kommunia"
      ),
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
        label = ~paste0(
          NAME_1, ": ",
          ifelse(is.na(total_guests), "No data", format(total_guests, big.mark = ","))
        ),
        popup = ~paste0(
          "<strong>", NAME_1, "</strong><br>",
          "Guests: ",
          ifelse(is.na(total_guests), "No data", format(total_guests, big.mark = ",")),
          "<br>Year: ", input$map_year
        )
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
  
  output$race_top <- renderText({
    top_row <- guests_per_region %>% arrange(desc(total_guests)) %>% slice(1)
    paste0(
      "Top region overall: ",
      top_row$region,
      " (",
      format(top_row$total_guests, big.mark = ","),
      " guests)."
    )
  })
  
  output$race_table <- DT::renderDT({
    guests_per_region %>%
      arrange(desc(total_guests)) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, autoWidth = TRUE)
      )
  })
  
  output$avg_trend_plot <- renderPlot({
    avg_stay %>%
      group_by(time) %>%
      summarise(avg_stays = mean(overnight_stays, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = time, y = avg_stays)) +
      geom_line(color = "darkgreen", size = 1.5) +
      geom_point(color = "darkgreen", size = 3) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
      theme_minimal() +
      labs(x = "Year", y = "Mean Stay Duration")
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
      labs(x = "Year", y = "Total Guests", color = "Region") +
      theme(
        strip.text = element_text(size = 10, face = "bold"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      guides(color = guide_legend(nrow = 2))
  })
}

# Run app
shinyApp(ui, server)

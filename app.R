
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readstata13)
library(lubridate)
library(leaflet)
library(sf)
library(RColorBrewer)
library(stringr)
library(shinycssloaders)
library(DT)
library(shinyWidgets)
library(htmltools)
library(plotly)
library(shinyjs)
library(base64enc)
library(shinyBS)
library(jquerylib)
library(Polychrome)

# Create custom color palette with 80 colors for up to 76 sites
P80 <- createPalette(80, c("#ff0000", "#00ff00", "#0000ff", "#FFFFFF"))

# Set global ggplot2 theme
theme_set(theme_minimal(base_size = 14) +
            theme(legend.position = "bottom",
                  plot.title = element_text(hjust = 0.5, face = "bold", color = "#333333")))

# --- Data Loading and Preprocessing ---
load_data <- function() {
  tryCatch({
    # Load UMSP data
    UMSP_data <- read.dta13(
      "E:/bbrsh/Desktop/Shakira/IDRC/Projects/UMSP dashboards/Monthly data for all sites through December 2024.dta",
      convert.factors = TRUE,
      generate.factors = TRUE,
      encoding = "UTF-8",
      convert.underscore = FALSE,
      missing.type = TRUE,
      convert.dates = TRUE,
      replace.strl = TRUE,
      nonint.factors = TRUE
    )
    
    # Load shapefile
    uganda_shapefile <- st_read(
      "E:/bbrsh/Desktop/Shakira/IDRC/Projects/TESA/NG GIS work/Shaky and Reuben projects/NG/Uganda districts shape files/Uganda_Districts-2020---136-wgs84.shp",
      quiet = TRUE
    ) %>%
      rename(district = dname2019) %>%
      dplyr::select(district, geometry) %>%
      mutate(district = str_to_upper(district)) %>%
      st_transform(crs = 4326)
    
    list(UMSP_data = UMSP_data, uganda_shapefile = uganda_shapefile)
  }, error = function(e) {
    stop("Error loading data: ", e$message)
  })
}

# Load and preprocess data
data <- load_data()
UMSP_data <- data$UMSP_data
uganda_shapefile <- data$uganda_shapefile

# Preprocess UMSP_data
UMSP_data <- UMSP_data %>%
  mutate(
    monthyear = as.Date(monthyear),
    Year = lubridate::year(monthyear),
    Year2 = as.character(Year),
    Region = as.factor(Region),
    NEWsiteID = as.factor(NEWsiteID),
    district = str_extract(NEWsiteID, "\\((.*?)\\)") %>%
      str_replace_all("[\\(\\)]", "") %>%
      str_replace(" District", "") %>%
      str_to_upper(),
    Site = as.character(NEWsiteID),
    Region = as.character(Region)
  )

# Function to categorize continuous data into quartiles
categorize_indicator <- function(x) {
  levels <- c("Low", "Medium-Low", "Medium-High", "High")
  result <- factor(rep(NA, length(x)), levels = levels)
  
  if (all(is.na(x))) return(result)
  
  x_non_na <- x[!is.na(x)]
  if (length(unique(x_non_na)) < 2) {
    result[!is.na(x)] <- "Low"
    return(result)
  }
  
  breaks <- quantile(x_non_na, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  if (length(unique(breaks)) < length(breaks)) {
    result[!is.na(x)] <- "Low"
    return(result)
  }
  
  result <- cut(x,
                breaks = breaks,
                labels = levels,
                include.lowest = TRUE,
                right = TRUE)
  result
}

# Define indicators
indicators <- c(
  "Malaria Incidence per 1000",
  "Proportion Suspected Malaria",
  "Proportion Suspected Malaria Tested",
  "Proportion Tested with RDT"
)

# Prepare site-level data
site_data_monthly <- UMSP_data %>%
  arrange(NEWsiteID, monthyear) %>%
  rename(
    "Malaria Incidence per 1000" = MI1000,
    "Proportion Suspected Malaria" = propsuspected,
    "Proportion Suspected Malaria Tested" = proptested,
    "Proportion Tested with RDT" = propRDT
  ) %>%
  dplyr::select(monthyear, Year, Region, Site, district, all_of(indicators)) %>%
  mutate(Level = "Site") %>%
  mutate(across(all_of(indicators), categorize_indicator, .names = "{.col}_cat"))

site_data_annual <- site_data_monthly %>%
  group_by(Year, Region, Site, district) %>%
  summarise(across(all_of(indicators), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(Level = "Site") %>%
  mutate(across(all_of(indicators), categorize_indicator, .names = "{.col}_cat"))

# Prepare region-level data
region_data_monthly <- UMSP_data %>%
  group_by(Region, monthyear) %>%
  summarise(
    `Malaria Incidence per 1000` = mean(MI1000, na.rm = TRUE),
    `Proportion Suspected Malaria` = sum(malariasuspected, na.rm = TRUE) / sum(visits, na.rm = TRUE),
    `Proportion Suspected Malaria Tested` = sum(TPRdenom, na.rm = TRUE) / sum(malariasuspected, na.rm = TRUE),
    `Proportion Tested with RDT` = sum(RDT, na.rm = TRUE) / sum(TPRdenom, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Level = "Region") %>%
  filter(!is.na(monthyear) & !is.na(Region)) %>%
  mutate(across(all_of(indicators), categorize_indicator, .names = "{.col}_cat"))

region_data_annual <- region_data_monthly %>%
  mutate(Year = lubridate::year(monthyear)) %>%
  group_by(Year, Region) %>%
  summarise(across(all_of(indicators), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(Level = "Region") %>%
  mutate(across(all_of(indicators), categorize_indicator, .names = "{.col}_cat"))

# Prepare mapping data
UMSP_data_sf <- UMSP_data %>%
  left_join(uganda_shapefile, by = "district") %>%
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  filter(st_is_valid(geometry))

mapping_monthly_data <- UMSP_data_sf %>%
  group_by(district, monthyear, geometry) %>%
  summarise(
    `Malaria Incidence per 1000` = mean(MI1000, na.rm = TRUE),
    `Proportion Suspected Malaria` = sum(malariasuspected, na.rm = TRUE) / sum(visits, na.rm = TRUE),
    `Proportion Suspected Malaria Tested` = sum(TPRdenom, na.rm = TRUE) / sum(malariasuspected, na.rm = TRUE),
    `Proportion Tested with RDT` = sum(RDT, na.rm = TRUE) / sum(TPRdenom, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Level = "Site") %>%
  mutate(across(all_of(indicators), categorize_indicator, .names = "{.col}_cat"))

# Define available columns for table
available_columns <- c("monthyear", "Year", "Region", "Site", "district", indicators, paste0(indicators, "_cat"), "Level")

# ---------------------------- UI ----------------------------
ui <- dashboardPage(
  skin = "blue",
  title = "IDRC Malaria Surveillance Dashboard",
  
  dashboardHeader(
    title = tags$div(
      class = "logo-lg",
      style = "display: flex; align-items: left; font-family: 'Montserrat', sans-serif; padding: 10px;",
      tags$img(src = "idrc_logo.png", height = "50px", style = "margin-right: 15px;")
    )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("info-circle")),
      menuItem("Temporal Analysis", tabName = "temporal", icon = icon("calendar")),
      menuItem("Spatial Analysis", tabName = "spatial_map", icon = icon("globe-africa")),
      menuItem("Data Table", tabName = "data_table", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        body {
          font-family: 'Roboto', sans-serif;
          color: #333333;
          background-color: #F5F6F5;
        }
        .main-header {
          background-color: #00016B !important;
          color: #FFFFFF !important;
        }
        .main-header .logo {
          background-color: #00016B !important;
          padding: 0 !important;
        }
        .main-sidebar {
          background-color: #1A237E !important;
        }
        .sidebar-menu li a {
          color: #FFFFFF !important;
          font-size: 16px;
        }
        .sidebar-menu li a:hover {
          background-color: #26A69A !important;
          color: #FFFFFF !important;
        }
        .content-wrapper {
          background-color: #F5F6F5 !important;
        }
        .box, .well {
          background-color: #FFFFFF !important;
          border: 1px solid #E0E0E0;
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
          padding: 15px;
        }
        .shiny-notification {
          background-color: #26A69A;
          color: #FFFFFF;
          border-radius: 8px;
          padding: 10px 20px;
          font-size: 14px;
        }
        .btn {
          background-color: #26A69A !important;
          color: #FFFFFF !important;
          border-radius: 4px;
          padding: 8px 16px;
          font-size: 14px;
        }
        .btn:hover {
          background-color: #1E867C !important;
        }
        .selectize-input, .selectize-dropdown {
          background-color: #FFFFFF !important;
          border: 1px solid #E0E0E0 !important;
          border-radius: 4px;
          font-size: 14px;
        }
        .plotly, .leaflet-container, .dataTables_wrapper {
          border-radius: 8px;
          background-color: #FFFFFF;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button {
          background-color: #26A69A !important;
          color: #FFFFFF !important;
          border-radius: 4px;
        }
        .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
          background-color: #1E867C !important;
        }
        h2, h3, h4 {
          font-family: 'Montserrat', sans-serif;
          color: #333333;
        }
        .footer {
          margin-top: 30px;
          text-align: center;
          color: #6A7F98;
          font-size: 12px;
          padding: 20px;
          background-color: #F5F6F5;
        }
        .footer hr {
          border-color: #F7A800;
          margin: 20px 0;
        }
        .fullscreen {
          width: 100vw !important;
          height: 100vh !important;
          position: fixed !important;
          top: 0;
          left: 0;
          z-index: 9999;
          background: white;
          padding: 20px;
          box-sizing: border-box;
          overflow: auto;
        }
        #temporal_wrapper, #spatial_wrapper, #table_wrapper {
          width: 100%;
          height: 600px;
          background: white;
          padding: 10px;
          overflow: auto;
        }
        #temporal_wrapper.fullscreen > div {
          width: 100% !important;
          height: 85vh !important;
        }
        #spatial_wrapper.fullscreen > div {
          width: 100% !important;
          height: 85vh !important;
        }
        #table_wrapper.fullscreen {
          width: 100% !important;
          max-height: 85vh !important;
          overflow-y: auto;
        }
        .exit-fullscreen-btn {
          position: fixed;
          top: 20px;
          right: 20px;
          z-index: 10001;
          padding: 12px 24px;
          background-color: #dc3545;
          color: white;
          font-size: 16px;
          font-weight: bold;
          border: none;
          border-radius: 6px;
          cursor: pointer;
          transition: background-color 0.2s;
          display: none;
        }
        .exit-fullscreen-btn:hover {
          background-color: #c82333;
        }
        .exit-fullscreen-btn:active {
          background-color: #bd2130;
        }
        .fullscreen .exit-fullscreen-btn {
          display: block !important;
        }
        .fullscreen .main-header,
        .fullscreen .main-sidebar,
        .fullscreen .content-wrapper > :not(.fullscreen),
        .fullscreen .footer {
          display: none !contribution
        }
        .content-wrapper {
          display: block !important;
        }
      ")),
      tags$script(HTML("
        // Temporal fullscreen functions
        function openFullscreenTemporal(elem, wrapperSelector) {
          console.log('Opening fullscreen for temporal:', wrapperSelector);
          $(wrapperSelector).addClass('fullscreen');
          if (elem.requestFullscreen) {
            elem.requestFullscreen().catch(err => console.error('Full-screen failed:', err));
          } else if (elem.mozRequestFullScreen) {
            elem.mozRequestFullScreen();
          } else if (elem.webkitRequestFullscreen) {
            elem.webkitRequestFullscreen();
          } else if (elem.msRequestFullscreen) {
            elem.msRequestFullscreen();
          }
          $('#exitbtn_temporal').show();
          Shiny.setInputValue('fullscreen_temporal_wrapper', true, {priority: 'event'});
          var plot = $(wrapperSelector).find('.plotly')[0];
          if (plot && typeof Plotly !== 'undefined') {
            Plotly.Plots.resize(plot);
            console.log('Resized Plotly plot');
          }
        }
        function exitFullscreenTemporal(wrapperSelector) {
          console.log('Exiting fullscreen for temporal:', wrapperSelector);
          if (document.fullscreenElement || document.webkitFullscreenElement || 
              document.mozFullScreenElement || document.msFullscreenElement) {
            if (document.exitFullscreen) {
              document.exitFullscreen().catch(err => console.error('Exit full-screen failed:', err));
            } else if (document.webkitExitFullscreen) {
              document.webkitExitFullscreen();
            } else if (document.msExitFullscreen) {
              document.msExitFullscreen();
            }
          }
          $(wrapperSelector).removeClass('fullscreen');
          $('#exitbtn_temporal').hide();
          Shiny.setInputValue('fullscreen_temporal_wrapper', false, {priority: 'event'});
          $('.main-header').show();
          $('.main-sidebar').show();
          $('.content-wrapper').show();
          $('.footer').show();
          var plot = $(wrapperSelector).find('.plotly')[0];
          if (plot && typeof Plotly !== 'undefined') {
            Plotly.Plots.resize(plot);
            console.log('Resized Plotly plot');
          }
        }
        // Spatial fullscreen functions
        function openFullscreenSpatial(elem, wrapperSelector) {
          console.log('Opening fullscreen for spatial:', wrapperSelector);
          $(wrapperSelector).addClass('fullscreen');
          if (elem.requestFullscreen) {
            elem.requestFullscreen().catch(err => console.error('Full-screen failed:', err));
          } else if (elem.mozRequestFullScreen) {
            elem.mozRequestFullScreen();
          } else if (elem.webkitRequestFullscreen) {
            elem.webkitRequestFullscreen();
          } else if (elem.msRequestFullscreen) {
            elem.msRequestFullscreen();
          }
          $('#exitbtn_spatial').show();
          Shiny.setInputValue('fullscreen_spatial_wrapper', true, {priority: 'event'});
          setTimeout(() => {
            if (window.leafletMap) {
              window.leafletMap.invalidateSize();
              console.log('Resized Leaflet map');
            }
          }, 100);
        }
        function exitFullscreenSpatial(wrapperSelector) {
          console.log('Exiting fullscreen for spatial:', wrapperSelector);
          if (document.fullscreenElement || document.webkitFullscreenElement || 
              document.mozFullScreenElement || document.msFullscreenElement) {
            if (document.exitFullscreen) {
              document.exitFullscreen().catch(err => console.error('Exit full-screen failed:', err));
            } else if (document.webkitExitFullscreen) {
              document.webkitExitFullscreen();
            } else if (document.msExitFullscreen) {
              document.msExitFullscreen();
            }
          }
          $(wrapperSelector).removeClass('fullscreen');
          $('#exitbtn_spatial').hide();
          Shiny.setInputValue('fullscreen_spatial_wrapper', false, {priority: 'event'});
          $('.main-header').show();
          $('.main-sidebar').show();
          $('.content-wrapper').show();
          $('.footer').show();
          setTimeout(() => {
            if (window.leafletMap) {
              window.leafletMap.invalidateSize();
              console.log('Resized Leaflet map');
            }
          }, 100);
        }
        // Table fullscreen functions
        function openFullscreenTable(elem, wrapperSelector) {
          console.log('Opening fullscreen for table:', wrapperSelector);
          $(wrapperSelector).addClass('fullscreen');
          if (elem.requestFullscreen) {
            elem.requestFullscreen().catch(err => console.error('Full-screen failed:', err));
          } else if (elem.mozRequestFullScreen) {
            elem.mozRequestFullScreen();
          } else if (elem.webkitRequestFullscreen) {
            elem.webkitRequestFullscreen();
          } else if (elem.msRequestFullscreen) {
            elem.msRequestFullscreen();
          }
          $('#exitbtn_table').show();
          Shiny.setInputValue('fullscreen_table_wrapper', true, {priority: 'event'});
        }
        function exitFullscreenTable(wrapperSelector) {
          console.log('Exiting fullscreen for table:', wrapperSelector);
          if (document.fullscreenElement || document.webkitFullscreenElement || 
              document.mozFullScreenElement || document.msFullscreenElement) {
            if (document.exitFullscreen) {
              document.exitFullscreen().catch(err => console.error('Exit full-screen failed:', err));
            } else if (document.webkitExitFullscreen) {
              document.webkitExitFullscreen();
            } else if (document.msExitFullscreen) {
              document.msExitFullscreen();
            }
          }
          $(wrapperSelector).removeClass('fullscreen');
          $('#exitbtn_table').hide();
          Shiny.setInputValue('fullscreen_table_wrapper', false, {priority: 'event'});
          $('.main-header').show();
          $('.main-sidebar').show();
          $('.content-wrapper').show();
          $('.footer').show();
        }
        // Handle browser-initiated fullscreen exit
        document.addEventListener('fullscreenchange', function() {
          if (!document.fullscreenElement) {
            var temporalWrapper = $('#temporal_wrapper.fullscreen');
            var spatialWrapper = $('#spatial_wrapper.fullscreen');
            var tableWrapper = $('#table_wrapper.fullscreen');
            if (temporalWrapper.length) {
              exitFullscreenTemporal('#temporal_wrapper');
              console.log('Browser exited fullscreen for temporal');
            } else if (spatialWrapper.length) {
              exitFullscreenSpatial('#spatial_wrapper');
              console.log('Browser exited fullscreen for spatial');
            } else if (tableWrapper.length) {
              exitFullscreenTable('#table_wrapper');
              console.log('Browser exited fullscreen for table');
            }
          }
        });
        document.addEventListener('webkitfullscreenchange', function() {
          if (!document.webkitFullscreenElement) {
            var temporalWrapper = $('#temporal_wrapper.fullscreen');
            var spatialWrapper = $('#spatial_wrapper.fullscreen');
            var tableWrapper = $('#table_wrapper.fullscreen');
            if (temporalWrapper.length) {
              exitFullscreenTemporal('#temporal_wrapper');
              console.log('Browser exited fullscreen for temporal');
            } else if (spatialWrapper.length) {
              exitFullscreenSpatial('#spatial_wrapper');
              console.log('Browser exited fullscreen for spatial');
            } else if (tableWrapper.length) {
              exitFullscreenTable('#table_wrapper');
              console.log('Browser exited fullscreen for table');
            }
          }
        });
        document.addEventListener('mozfullscreenchange', function() {
          if (!document.mozFullScreenElement) {
            var temporalWrapper = $('#temporal_wrapper.fullscreen');
            var spatialWrapper = $('#spatial_wrapper.fullscreen');
            var tableWrapper = $('#table_wrapper.fullscreen');
            if (temporalWrapper.length) {
              exitFullscreenTemporal('#temporal_wrapper');
              console.log('Browser exited fullscreen for temporal');
            } else if (spatialWrapper.length) {
              exitFullscreenSpatial('#spatial_wrapper');
              console.log('Browser exited fullscreen for spatial');
            } else if (tableWrapper.length) {
              exitFullscreenTable('#table_wrapper');
              console.log('Browser exited fullscreen for table');
            }
          }
        });
        document.addEventListener('MSFullscreenChange', function() {
          if (!document.msFullscreenElement) {
            var temporalWrapper = $('#temporal_wrapper.fullscreen');
            var spatialWrapper = $('#spatial_wrapper.fullscreen');
            var tableWrapper = $('#table_wrapper.fullscreen');
            if (temporalWrapper.length) {
              exitFullscreenTemporal('#temporal_wrapper');
              console.log('Browser exited fullscreen for temporal');
            } else if (spatialWrapper.length) {
              exitFullscreenSpatial('#spatial_wrapper');
              console.log('Browser exited fullscreen for spatial');
            } else if (tableWrapper.length) {
              exitFullscreenTable('#table_wrapper');
              console.log('Browser exited fullscreen for table');
            }
          }
        });
      "))
    ),
    
    tabItems(
      tabItem(tabName = "introduction",
              h2("IDRC Malaria Surveillance Dashboard"),
              div(style = "margin-top: 20px;",
                  div(style = "background-color: #F1F8FC; padding: 20px; border-radius: 10px;",
                      h4("Welcome to the Malaria Surveillance Dashboard", style = "color: #003366; font-weight: bold;"),
                      p("This interactive dashboard provides insights into malaria trends including key metrics such as incidence, suspected cases, and testing rates. Use the filters to explore trends over time.",
                        style = "font-size: 16px; color: #555555;"),
                      p("Explore the charts and summary tables to understand malaria surveillance data in-depth.",
                        style = "font-size: 16px; color: #555555;")
                  ),
                  div(style = "background-color: #FFF0D4; padding: 20px; margin-top: 20px; border-radius: 10px;",
                      h4("Key Features of This Dashboard", style = "color: #F7A800; font-weight: bold;"),
                      p("1. Real-time malaria trends by region and time.", style = "font-size: 16px; color: #555555;"),
                      p("2. Regional disparities in incidence and testing rates.", style = "font-size: 16px; color: #555555;"),
                      p("3. Customizable filters for tailored analysis.", style = "font-size: 16px; color: #555555;")
                  ),
                  div(style = "margin-top: 20px; text-align: center;",
                      tags$a(href = "https://www.idrc-uganda.org/",
                             "Visit the IDRC Uganda Website",
                             target = "_blank",
                             style = "font-size: 16px; color: #0066cc; font-weight: bold; text-decoration: underline;")
                  )
              )
      ),
      
      tabItem(tabName = "temporal",
              h2("Temporal Analysis"),
              fluidRow(
                column(3,
                       div(
                         h4(icon("chart-line"), "Visualization Options",
                            style = "color: #000000; border-bottom: 2px solid #F7A800; padding-bottom: 8px; font-weight: bold;"),
                         selectInput("level", "Select Level:",
                                     choices = c("Region", "Site"),
                                     selected = "Region"),
                         selectInput("time_scale", "Select Time Scale:",
                                     choices = c("Annual", "Monthly"),
                                     selected = "Annual"),
                         selectizeInput("selected_entity", "Select Region(s)/Site(s):",
                                        choices = NULL,
                                        multiple = TRUE,
                                        options = list(placeholder = "Select regions or sites")),
                         selectInput("variable", "Choose Variable:",
                                     choices = indicators,
                                     selected = indicators[1]),
                         selectInput("plot_type", "Choose Plot Type:",
                                     choices = c("Line", "Bar", "Heatmap"),
                                     selected = "Line"),
                         conditionalPanel(
                           condition = "input.plot_type == 'Heatmap'",
                           selectInput("heatmap_data_type", "Heatmap Data Type:",
                                       choices = c("Continuous", "Categorical"),
                                       selected = "Continuous")
                         ),
                         selectInput("facet_type", "Choose Display Type:",
                                     choices = c("Combined", "Faceted"),
                                     selected = "Combined"),
                         uiOutput("time_selector"),
                         actionButton("fs_temporal",
                                      "Full Screen",
                                      icon = icon("expand"),
                                      onclick = "openFullscreenTemporal(document.getElementById('temporal_wrapper'), '#temporal_wrapper');")
                       ),
                       div(
                         h4(icon("download"), "Download Options",
                            style = "color: #000000; border-bottom: 2px solid #F7A800; padding-bottom: 8px; font-weight: bold;"),
                         actionButton("reset_filters", "Reset Filters", icon = icon("redo")),
                         downloadButton("download_plot", "Download Plot"),
                         downloadButton("download_data", "Download Data")
                       )
                ),
                column(9,
                       div(id = "temporal_wrapper",
                           withSpinner(plotlyOutput("temporal_plot", height = "600px")),
                           actionButton("exitbtn_temporal",
                                        "Exit",
                                        class = "exit-fullscreen-btn",
                                        onclick = "exitFullscreenTemporal('#temporal_wrapper');")
                       )
                )
              )
      ),
      
      tabItem(tabName = "spatial_map",
              h2("District-Level Malaria Map"),
              fluidRow(
                column(3,
                       div(
                         h4(icon("chart-line"), "Visualization Options",
                            style = "color: #000000; border-bottom: 2px solid #F7A800; padding-bottom: 8px; font-weight: bold;"),
                         selectInput("map_time_scale", "Select Time Scale:",
                                     choices = c("Years", "Months"),
                                     selected = "Years"),
                         conditionalPanel(
                           condition = "input.map_time_scale == 'Years'",
                           pickerInput("map_years", "Select Year(s):",
                                       choices = sort(unique(lubridate::year(mapping_monthly_data$monthyear))),
                                       selected = character(0),
                                       multiple = TRUE,
                                       options = list(`actions-box` = TRUE, `live-search` = TRUE))
                         ),
                         conditionalPanel(
                           condition = "input.map_time_scale == 'Months'",
                           dateRangeInput("map_date_range", "Select Date Range:",
                                          start = min(mapping_monthly_data$monthyear, na.rm = TRUE),
                                          end = max(mapping_monthly_data$monthyear, na.rm = TRUE),
                                          min = min(mapping_monthly_data$monthyear, na.rm = TRUE),
                                          max = max(mapping_monthly_data$monthyear, na.rm = TRUE),
                                          format = "yyyy-mm")
                         ),
                         selectizeInput("selected_district_map", "Select District(s):",
                                        choices = c("All", sort(unique(mapping_monthly_data$district))),
                                        selected = "All",
                                        multiple = TRUE,
                                        options = list(placeholder = "Select districts")),
                         selectInput("selected_metric", "Select Metric:",
                                     choices = indicators,
                                     selected = indicators[1]),
                         selectInput("map_data_type", "Map Data Type:",
                                     choices = c("Continuous", "Categorical"),
                                     selected = "Continuous"),
                         conditionalPanel(
                           condition = "input.map_data_type == 'Categorical'",
                           uiOutput("categorical_ranges")
                         ),
                         actionButton("fs_spatial",
                                      "Full Screen",
                                      icon = icon("expand"),
                                      onclick = "openFullscreenSpatial(document.getElementById('spatial_wrapper'), '#spatial_wrapper');")
                       ),
                       div(
                         h4(icon("download"), "Download Options",
                            style = "color: #000000; border-bottom: 2px solid #F7A800; padding-bottom: 8px; font-weight: bold;"),
                         downloadButton("download_map", "Download Map")
                       )
                ),
                column(9,
                       div(id = "spatial_wrapper",
                           withSpinner(leafletOutput("malaria_map", height = "600px")),
                           actionButton("exitbtn_spatial",
                                        "Exit",
                                        class = "exit-fullscreen-btn",
                                        onclick = "exitFullscreenSpatial('#spatial_wrapper');")
                       )
                )
              )
      ),
      
      tabItem(tabName = "data_table",
              h2("Data Table"),
              fluidRow(
                column(3,
                       div(
                         h4(icon("table"), "Data selection Options",
                            style = "color: #000000; border-bottom: 2px solid #F7A800; padding-bottom: 8px; font-weight: bold;"),
                         selectInput("table_level", "Select Level:",
                                     choices = c("Region", "Site"),
                                     selected = "Region"),
                         selectInput("table_time_scale", "Select Time Scale:",
                                     choices = c("Annual", "Monthly"),
                                     selected = "Annual"),
                         conditionalPanel(
                           condition = "input.table_level == 'Region' || input.table_level == 'Site'",
                           selectizeInput("table_selected_region", "Select Region(s):",
                                          choices = c("All", sort(unique(UMSP_data$Region))),
                                          selected = "All",
                                          multiple = TRUE,
                                          options = list(placeholder = "Select regions"))
                         ),
                         conditionalPanel(
                           condition = "input.table_level == 'Site'",
                           selectizeInput("table_selected_site", "Select Site(s):",
                                          choices = c("All", sort(unique(UMSP_data$Site))),
                                          selected = "All",
                                          multiple = TRUE,
                                          options = list(placeholder = "Select sites"))
                         ),
                         selectizeInput("table_selected_district", "Select District(s):",
                                        choices = c("All", sort(unique(UMSP_data$district))),
                                        selected = "All",
                                        multiple = TRUE,
                                        options = list(placeholder = "Select districts")),
                         conditionalPanel(
                           condition = "input.table_time_scale == 'Annual'",
                           pickerInput("table_years", "Select Year(s):",
                                       choices = c("All", sort(unique(UMSP_data$Year))),
                                       selected = "All",
                                       multiple = TRUE,
                                       options = list(`actions-box` = TRUE, `live-search` = TRUE))
                         ),
                         conditionalPanel(
                           condition = "input.table_time_scale == 'Monthly'",
                           dateRangeInput("table_date_range", "Select Date Range:",
                                          start = min(region_data_monthly$monthyear, na.rm = TRUE),
                                          end = max(region_data_monthly$monthyear, na.rm = TRUE),
                                          min = min(region_data_monthly$monthyear, na.rm = TRUE),
                                          max = max(region_data_monthly$monthyear, na.rm = TRUE),
                                          format = "yyyy-mm")
                         ),
                         pickerInput("table_columns", "Select Columns to Display:",
                                     choices = available_columns,
                                     selected = available_columns,
                                     multiple = TRUE,
                                     options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                         actionButton("fs_table",
                                      "Full Screen",
                                      icon = icon("expand"),
                                      onclick = "openFullscreenTable(document.getElementById('table_wrapper'), '#table_wrapper');")
                       ),
                       div(
                         h4(icon("download"), "Download Options",
                            style = "color: #000000; border-bottom: 2px solid #F7A800; padding-bottom: 8px; font-weight: bold;"),
                         downloadButton("download_table", "Download Table")
                       )
                ),
                column(9,
                       div(id = "table_wrapper",
                           withSpinner(DTOutput("data_table")),
                           actionButton("exitbtn_table",
                                        "Exit",
                                        class = "exit-fullscreen-btn",
                                        onclick = "exitFullscreenTable('#table_wrapper');")
                       )
                )
              )
      )
    ),
    div(class = "footer",
        hr(style = "border-color: #F7A800; margin: 20px 0;"),
        p("IDRC Uganda - Malaria Surveillance Program"),
        p(icon("calendar"), " Data last updated: ", textOutput("last_updated", inline = TRUE))
    )
  )
)

# ---------------------------- Server ----------------------------
server <- function(input, output, session) {
  
  # --- Initialize Reactive Values ---
  active_tab <- reactiveVal("introduction")  # Default to introduction
  
  # --- Track Active Tab ---
  observeEvent(input$tabs, {
    active_tab(input$tabs)
  })
  
  # --- Dynamic Filtered Data ---
  filtered_data <- reactive({
    tab <- active_tab()
    switch(tab,
           "temporal" = filtered_temporal_data(),
           "spatial_map" = filtered_map_data() %>% st_drop_geometry(),
           "data_table" = filtered_table_data())
  })
  
  # --- Last Updated Date ---
  output$last_updated <- renderText({
    last_date <- max(UMSP_data$monthyear, na.rm = TRUE)
    format(last_date, "%B %Y")
  })
  
  # --- Update Selectize Inputs ---
  observe({
    choices <- if (input$level == "Region") {
      c("All", sort(unique(region_data_annual$Region)))
    } else {
      c("All", sort(unique(site_data_annual$Site)))
    }
    updateSelectizeInput(session, "selected_entity", choices = choices, selected = "All")
  })
  
  # --- Temporal Analysis ---
  output$time_selector <- renderUI({
    if (input$time_scale == "Annual") {
      sliderInput("year_range", "Select Year Range:",
                  min = min(region_data_annual$Year, na.rm = TRUE),
                  max = max(region_data_annual$Year, na.rm = TRUE),
                  value = c(min(region_data_annual$Year, na.rm = TRUE), max(region_data_annual$Year, na.rm = TRUE)),
                  step = 1, sep = "")
    } else {
      dateRangeInput("date_range", "Select Date Range:",
                     start = min(region_data_monthly$monthyear, na.rm = TRUE),
                     end = max(region_data_monthly$monthyear, na.rm = TRUE),
                     min = min(region_data_monthly$monthyear, na.rm = TRUE),
                     max = max(region_data_monthly$monthyear, na.rm = TRUE),
                     format = "yyyy-mm")
    }
  })
  
  filtered_temporal_data <- reactive({
    data <- switch(paste(input$level, input$time_scale, sep = "_"),
                   "Region_Annual" = region_data_annual,
                   "Region_Monthly" = region_data_monthly,
                   "Site_Annual" = site_data_annual,
                   site_data_monthly)
    
    if (!"All" %in% input$selected_entity) {
      data <- data %>% filter(if (input$level == "Region") Region %in% input$selected_entity else Site %in% input$selected_entity)
    }
    
    if (input$time_scale == "Annual") {
      data <- data %>% filter(Year >= input$year_range[1] & Year <= input$year_range[2])
    } else {
      data <- data %>% filter(monthyear >= as.Date(input$date_range[1]) & monthyear <= as.Date(input$date_range[2]))
    }
    
    data
  })
  
  output$temporal_plot <- renderPlotly({
    data <- filtered_temporal_data()
    if (nrow(data) == 0) {
      showNotification("No data available for the selected filters.", type = "warning")
      return(plot_ly() %>% add_text(x = 0.5, y = 0.5, text = "No data available", textfont = list(size = 20)))
    }
    
    x_var <- if (input$time_scale == "Annual") "Year" else "monthyear"
    group_var <- if (input$level == "Region") "Region" else "Site"
    group_levels <- unique(data[[group_var]])
    num_groups <- length(group_levels)
    colors <- setNames(P80[1:num_groups], group_levels)  # Map colors to group levels
    
    p <- if (input$plot_type == "Heatmap" && input$heatmap_data_type == "Categorical") {
      value_var <- paste0(input$variable, "_cat")
      ggplot(data, aes(x = .data[[x_var]], y = .data[[group_var]], fill = .data[[value_var]])) +
        geom_tile() +
        scale_fill_manual(values = setNames(brewer.pal(4, "Set2"), c("Low", "Medium-Low", "Medium-High", "High")),
                          na.value = "#f0f0f0") +
        labs(fill = paste(input$variable, "(Categorical)"))
    } else if (input$plot_type == "Heatmap") {
      ggplot(data, aes(x = .data[[x_var]], y = .data[[group_var]], fill = .data[[input$variable]])) +
        geom_tile() +
        scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd")) +
        labs(fill = input$variable)
    } else if (input$plot_type == "Bar") {
      ggplot(data, aes(x = .data[[x_var]], y = .data[[input$variable]], fill = .data[[group_var]])) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = colors)
    } else {
      ggplot(data, aes(x = .data[[x_var]], y = .data[[input$variable]], color = .data[[group_var]])) +
        geom_line(size = 1) + 
        geom_point() +
        scale_color_manual(values = colors)
    }
    
    if (input$plot_type != "Heatmap" && input$facet_type == "Faceted") {
      p <- p + facet_wrap(as.formula(paste("~", group_var)), scales = "free_y")
    }
    
    p <- p +
      labs(title = paste(input$time_scale, input$variable, "per", input$level),
           y = input$variable) +
      theme(legend.position = if (input$facet_type == "Faceted" && input$plot_type != "Heatmap") "none" else "bottom")
    
    ggplotly(p, tooltip = c("x", "y", group_var))
  })
  
  output$download_plot <- downloadHandler(
    filename = function() paste("temporal_plot_", input$variable, "_", Sys.Date(), ".png", sep = ""),
    content = function(file) {
      data <- filtered_temporal_data()
      x_var <- if (input$time_scale == "Annual") "Year" else "monthyear"
      group_var <- if (input$level == "Region") "Region" else "Site"
      group_levels <- unique(data[[group_var]])
      num_groups <- length(group_levels)
      colors <- setNames(P80[1:num_groups], group_levels)  # Map colors to group levels
      
      p <- if (input$plot_type == "Heatmap" && input$heatmap_data_type == "Categorical") {
        value_var <- paste0(input$variable, "_cat")
        ggplot(data, aes(x = .data[[x_var]], y = .data[[group_var]], fill = .data[[value_var]])) +
          geom_tile() +
          scale_fill_manual(values = setNames(brewer.pal(4, "Set2"), c("Low", "Medium-Low", "Medium-High", "High")),
                            na.value = "#f0f0f0") +
          labs(fill = paste(input$variable, "(Categorical)"))
      } else if (input$plot_type == "Heatmap") {
        ggplot(data, aes(x = .data[[x_var]], y = .data[[group_var]], fill = .data[[input$variable]])) +
          geom_tile() +
          scale_fill_gradientn(colors = brewer.pal(9, "YlOrRd")) +
          labs(fill = input$variable)
      } else if (input$plot_type == "Bar") {
        ggplot(data, aes(x = .data[[x_var]], y = .data[[input$variable]], fill = .data[[group_var]])) +
          geom_bar(stat = "identity", position = "dodge") +
          scale_fill_manual(values = colors)
      } else {
        ggplot(data, aes(x = .data[[x_var]], y = .data[[input$variable]], color = .data[[group_var]])) +
          geom_line(size = 1) +
          geom_point() +
          scale_color_manual(values = colors)
      }
      
      if (input$plot_type != "Heatmap" && input$facet_type == "Faceted") {
        p <- p + facet_wrap(as.formula(paste("~", group_var)), scales = "free_y")
      }
      
      p <- p +
        labs(title = paste(input$time_scale, input$variable, "per", input$level),
             y = input$variable) +
        theme(legend.position = if (input$facet_type == "Faceted" && input$plot_type != "Heatmap") "none" else "bottom")
      
      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() paste("temporal_data_", input$variable, "_", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(filtered_temporal_data(), file, row.names = FALSE)
  )
  
  # --- Spatial Analysis ---
  filtered_map_data <- reactive({
    metric <- input$selected_metric
    
    data <- mapping_monthly_data
    
    # Filter based on time scale
    if (input$map_time_scale == "Years") {
      if (length(input$map_years) > 0) {
        data <- data %>% 
          filter(lubridate::year(monthyear) %in% as.numeric(input$map_years))
      } else {
        # Return empty data if no years are selected
        return(data[0, ])
      }
    } else { # Months
      data <- data %>% 
        filter(monthyear >= as.Date(input$map_date_range[1]) & 
                 monthyear <= as.Date(input$map_date_range[2]))
    }
    
    # Compute quartile breaks on filtered data before aggregation
    breaks <- if (!all(is.na(data[[metric]])) && length(unique(data[[metric]][!is.na(data[[metric]])])) >= 2) {
      quantile(data[[metric]], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    } else {
      rep(NA, 5)  # Default breaks if insufficient data
    }
    
    # Aggregate data over the selected time period
    data <- data %>%
      group_by(district, geometry) %>%
      summarise(
        `Malaria Incidence per 1000` = mean(`Malaria Incidence per 1000`, na.rm = TRUE),
        `Proportion Suspected Malaria` = mean(`Proportion Suspected Malaria`, na.rm = TRUE),
        `Proportion Suspected Malaria Tested` = mean(`Proportion Suspected Malaria Tested`, na.rm = TRUE),
        `Proportion Tested with RDT` = mean(`Proportion Tested with RDT`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(across(all_of(indicators), categorize_indicator, .names = "{.col}_cat"))
    
    # Apply district filter
    if (!"All" %in% input$selected_district_map) {
      data <- data %>% 
        filter(district %in% input$selected_district_map)
    }
    
    # Add selected metric value
    data <- data %>%
      mutate(selected_metric_value = if (input$map_data_type == "Continuous") 
        .data[[metric]] 
        else 
          .data[[paste0(metric, "_cat")]])
    
    # Add range labels
    if (input$map_data_type == "Categorical") {
      data <- data %>%
        mutate(
          metric_range = case_when(
            is.na(selected_metric_value) ~ "No data",
            selected_metric_value == "Low" & !any(is.na(breaks)) ~ sprintf("Low (≤ %.2f)", breaks[2]),
            selected_metric_value == "Medium-Low" & !any(is.na(breaks)) ~ sprintf("Medium-Low (%.2f - %.2f)", breaks[2], breaks[3]),
            selected_metric_value == "Medium-High" & !any(is.na(breaks)) ~ sprintf("Medium-High (%.2f - %.2f)", breaks[3], breaks[4]),
            selected_metric_value == "High" & !any(is.na(breaks)) ~ sprintf("High (> %.2f)", breaks[4]),
            TRUE ~ "No data"
          )
        )
    } else {
      data <- data %>% mutate(metric_range = ifelse(is.na(selected_metric_value), "No data", sprintf("%.2f", selected_metric_value)))
    }
    
    data
  })
  
  output$categorical_ranges <- renderUI({
    req(input$map_data_type == "Categorical")
    metric <- input$selected_metric
    data <- mapping_monthly_data
    
    # Apply same time filters as in filtered_map_data
    if (input$map_time_scale == "Years") {
      if (length(input$map_years) > 0) {
        data <- data %>% 
          filter(lubridate::year(monthyear) %in% as.numeric(input$map_years))
      } else {
        return(p("Please select at least one year to display categorical ranges."))
      }
    } else { # Months
      data <- data %>% 
        filter(monthyear >= as.Date(input$map_date_range[1]) & 
                 monthyear <= as.Date(input$map_date_range[2]))
    }
    
    data <- data %>% filter(!is.na(.data[[metric]]))
    
    if (nrow(data) == 0 || length(unique(data[[metric]])) < 2) {
      return(p("Insufficient data variation to calculate meaningful ranges."))
    }
    
    breaks <- quantile(data[[metric]], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    
    if (length(unique(breaks)) < 4) {
      return(p("Data has insufficient variation; all values are too similar to categorize into quartiles."))
    }
    
    tagList(
      h5("Categorical Ranges (Based on Quartiles):"),
      p(sprintf("Low: ≤ %.2f", breaks[2])),
      p(sprintf("Medium-Low: %.2f - %.2f", breaks[2], breaks[3])),
      p(sprintf("Medium-High: %.2f - %.2f", breaks[3], breaks[4])),
      p(sprintf("High: > %.2f", breaks[4]))
    )
  })
  
  output$malaria_map <- renderLeaflet({
    data <- filtered_map_data()
    metric <- input$selected_metric
    data_type <- input$map_data_type
    
    if (nrow(data) == 0 || all(is.na(data$selected_metric_value))) {
      showNotification("No data available for this selection. Please select at least one year.", type = "warning")
      return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron))
    }
    
    # Define palettes based on metric type (bad or good outcome)
    bad_outcomes <- c("Malaria Incidence per 1000", "Proportion Suspected Malaria")
    good_outcomes <- c("Proportion Suspected Malaria Tested", "Proportion Tested with RDT")
    
    pal <- if (data_type == "Continuous") {
      if (metric %in% bad_outcomes) {
        colorNumeric(palette = c("#FFFF00", "#FF0000"), domain = data$selected_metric_value, na.color = "#f0f0f0")
      } else {
        colorNumeric(palette = c("#FFFF00", "#00FF00"), domain = data$selected_metric_value, na.color = "#f0f0f0")
      }
    } else {
      if (metric %in% bad_outcomes) {
        colorFactor(
          palette = c("#FFFF99", "#FFCC66", "#FF6633", "#CC0000"),  # Yellow to red
          domain = levels(data$selected_metric_value),
          na.color = "#f0f0f0",
          levels = c("Low", "Medium-Low", "Medium-High", "High")
        )
      } else {
        colorFactor(
          palette = c("#FFFF99", "#CCFF66", "#66CC33", "#009900"),  # Yellow to green
          domain = levels(data$selected_metric_value),
          na.color = "#f0f0f0",
          levels = c("Low", "Medium-Low", "Medium-High", "High")
        )
      }
    }
    
    popups <- lapply(1:nrow(data), function(i) {
      district_data <- mapping_monthly_data %>%
        filter(district == data$district[i]) %>%
        { if (input$map_time_scale == "Years" && length(input$map_years) > 0) 
          filter(., lubridate::year(monthyear) %in% as.numeric(input$map_years))
          else if (input$map_time_scale == "Months") 
            filter(., monthyear >= as.Date(input$map_date_range[1]) & monthyear <= as.Date(input$map_date_range[2]))
          else .
        } %>%
        arrange(desc(monthyear)) %>%
        slice_head(n = 6)
      
      trend_img <- if (nrow(district_data) > 0 && !all(is.na(district_data$`Malaria Incidence per 1000`))) {
        trend_plot <- ggplot(district_data, aes(x = monthyear, y = `Malaria Incidence per 1000`)) +
          geom_line(color = "blue") +
          labs(title = paste("Monthly Malaria Incidence per 1000 in", data$district[i]), x = NULL, y = "Incidence per 1000") +
          theme_minimal(base_size = 10)
        trend_plot_svg <- tempfile(fileext = ".svg")
        svg(trend_plot_svg, width = 4, height = 2)
        print(trend_plot)
        dev.off()
        trend_plot_base64 <- base64enc::base64encode(readBin(trend_plot_svg, "raw", file.info(trend_plot_svg)$size))
        paste0("<img src='data:image/svg+xml;base64,", trend_plot_base64, "' width='200' height='100'/>")
      } else {
        "<p>No trend data available</p>"
      }
      
      HTML(paste0(
        "<strong>", data$district[i], "</strong><br>",
        metric, ": ", data$metric_range[i], "<br>",
        "Suspected Malaria: ", round(data$`Proportion Suspected Malaria`[i] * 100, 2), "%<br>",
        "Tested Proportion: ", round(data$`Proportion Suspected Malaria Tested`[i] * 100, 2), "%<br>",
        trend_img
      ))
    })
    
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(selected_metric_value),
        weight = 1,
        color = "white",
        fillOpacity = 0.8,
        label = ~paste0(
          district, ": ",
          metric_range
        ),
        popup = popups,
        highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE)
      ) %>%
      addLegend(
        pal = pal,
        values = ~selected_metric_value,
        title = paste0("<strong>", metric, if (data_type == "Categorical") " (Categorical)" else "", "</strong>"),
        position = "bottomright",
        na.label = "No data"
      ) %>%
      addControl(
        html = paste0(
          "<strong style='font-size:16px;'>", metric, if (data_type == "Categorical") " (Categorical)" else "",
          " in ", ifelse("All" %in% input$selected_district_map, "Different Districts", paste(input$selected_district_map, collapse = ", ")),
          "<br>", 
          if (input$map_time_scale == "Years") {
            paste(input$map_years, collapse = ", ")
          } else {
            paste(format(as.Date(input$map_date_range[1]), "%B %Y"), " to ", format(as.Date(input$map_date_range[2]), "%B %Y"))
          },
          "</strong>"
        ),
        position = "topright"
      )
  })
  
  output$download_map <- downloadHandler(
    filename = function() paste("malaria_map_", input$selected_metric, "_", Sys.Date(), ".html", sep = ""),
    content = function(file) {
      data <- filtered_map_data()
      metric <- input$selected_metric
      data_type <- input$map_data_type
      
      # Define palettes for download
      bad_outcomes <- c("Malaria Incidence per 1000", "Proportion Suspected Malaria")
      good_outcomes <- c("Proportion Suspected Malaria Tested", "Proportion Tested with RDT")
      
      pal <- if (data_type == "Continuous") {
        if (metric %in% bad_outcomes) {
          colorNumeric(palette = c("#FFFF00", "#FF0000"), domain = data$selected_metric_value, na.color = "#f0f0f0")
        } else {
          colorNumeric(palette = c("#FFFF00", "#00FF00"), domain = data$selected_metric_value, na.color = "#f0f0f0")
        }
      } else {
        if (metric %in% bad_outcomes) {
          colorFactor(
            palette = c("#FFFF99", "#FFCC66", "#FF6633", "#CC0000"),  # Yellow to red
            domain = levels(data$selected_metric_value),
            na.color = "#f0f0f0",
            levels = c("Low", "Medium-Low", "Medium-High", "High")
          )
        } else {
          colorFactor(
            
            palette = c("#FFFF99", "#CCFF66", "#66CC33", "#009900"),  # Yellow to green
            domain = levels(data$selected_metric_value),
            na.color = "#f0f0f0",
            levels = c("Low", "Medium-Low", "Medium-High", "High")
          )
        }
      }
      
      map <- leaflet(data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(selected_metric_value),
          weight = 1,
          color = "white",
          fillOpacity = 0.8,
          label = ~paste0(
            "District: ", district,
            "; ", metric, ": ",
            metric_range
          ),
          highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE)
        ) %>%
        addLegend(
          pal = pal,
          values = ~selected_metric_value,
          title = paste0(metric, if (data_type == "Categorical") " (Categorical)" else ""),
          position = "bottomright",
          na.label = "No data"
        )
      
      htmlwidgets::saveWidget(map, file)
    }
  )
  
  # --- Data Table ---
  filtered_table_data <- reactive({
    data <- switch(paste(input$table_level, input$table_time_scale, sep = "_"),
                   "Region_Annual" = region_data_annual,
                   "Region_Monthly" = region_data_monthly,
                   "Site_Annual" = site_data_annual,
                   site_data_monthly)
    
    if (!"All" %in% input$table_selected_region) {
      data <- data %>% filter(Region %in% input$table_selected_region)
    }
    
    if (input$table_level == "Site" && !"All" %in% input$table_selected_site) {
      data <- data %>% filter(Site %in% input$table_selected_site)
    }
    
    if (!"All" %in% input$table_selected_district) {
      data <- data %>% filter(district %in% input$table_selected_district)
    }
    
    if (input$table_time_scale == "Annual") {
      if (!"All" %in% input$table_years) {
        data <- data %>% filter(Year %in% as.numeric(input$table_years))
      }
    } else {
      data <- data %>% filter(monthyear >= as.Date(input$table_date_range[1]) & monthyear <= as.Date(input$table_date_range[2]))
    }
    
    if (nrow(data) == 0) {
      showNotification("No data available for the selected filters.", type = "warning")
    }
    
    data
  })
  
  output$data_table <- renderDT({
    data <- filtered_table_data()
    
    if (length(input$table_columns) == 0) {
      showNotification("Please select at least one column to display.", type = "warning")
      return(datatable(data.frame(Message = "No columns selected"), filter = "none"))
    }
    
    valid_columns <- intersect(input$table_columns, colnames(data))
    if (length(valid_columns) == 0) {
      showNotification("Selected columns are not available in the data.", type = "warning")
      return(datatable(data.frame(Message = "No valid columns selected"), filter = "none"))
    }
    
    display_data <- data %>% select(all_of(valid_columns))
    
    dt <- datatable(
      display_data,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      ),
      extensions = "Buttons",
      rownames = FALSE,
      caption = paste(input$table_time_scale, "Data by", input$table_level)
    )
    
    numeric_columns <- intersect(valid_columns, indicators)
    if (length(numeric_columns) > 0) {
      dt <- dt %>% formatRound(columns = numeric_columns, digits = 2)
    }
    
    dt
  })
  
  output$download_table <- downloadHandler(
    filename = function() paste("data_table_", input$table_level, "_", input$table_time_scale, "_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      data <- filtered_table_data()
      valid_columns <- intersect(input$table_columns, colnames(data))
      if (length(valid_columns) == 0) {
        showNotification("No valid columns selected for download.", type = "warning")
        return()
      }
      download_data <- data %>% select(all_of(valid_columns))
      write.csv(download_data, file, row.names = FALSE)
    }
  )
  
  # --- Reset Filters ---
  observeEvent(input$reset_filters, {
    updateSelectizeInput(session, "selected_entity", selected = "All")
    updateSelectInput(session, "level", selected = "Region")
    updateSelectInput(session, "time_scale", selected = "Annual")
    updateSelectInput(session, "variable", selected = indicators[1])
    updateSelectInput(session, "plot_type", selected = "Line")
    updateSelectInput(session, "facet_type", selected = "Combined")
    updateSelectInput(session, "heatmap_data_type", selected = "Continuous")
    if (input$time_scale == "Annual") {
      updateSliderInput(session, "year_range",
                        value = c(min(region_data_annual$Year, na.rm = TRUE), max(region_data_annual$Year, na.rm = TRUE)))
    } else {
      updateDateRangeInput(session, "date_range",
                           start = min(region_data_monthly$monthyear, na.rm = TRUE),
                           end = max(region_data_monthly$monthyear, na.rm = TRUE))
    }
  })
}

# ---------------------------- Run App ----------------------------
shinyApp(ui, server)

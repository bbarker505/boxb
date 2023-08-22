# Designed to run from day 1 of year through 4 days past current date
# For example, if today is 8/1/23, then runs for 1/1/23 to 8/5/23
library(tidyverse) # Data wrangling/manipulation
library(terra) # Import model outputs / work with rasters
library(raster) # TO DO: hopefully can remove this
library(sf) # Spatial features
library(mapview) # Open access street maps
library(tigris) # County and state boundaries
library(leafem) # Query map values
library(leaflet) # Interactive maps
library(leaflegend) # Extra legend features
library(leaflet.extras) 
library(leafsync) # Sync two leaflet maps (current year + last year)
library(lubridate) # Working with dates
library(tidygeocoder) # Obtain coordinates from address
library(shiny) # Web app 
library(shinyWidgets)
library(shinydashboard)
library(shinyBS) # Info tabs next to risk map menu items
library(rsconnect) # Share app via shinapp.io
library(bslib)
#library(shinybrowser) # Use to detect between tablet/phone and desktop??
library(fresh) # Color theme for web app page
library(htmlwidgets)

# TO DO: "“addImageQuery” only allows for raster and stars objects. Hopefully
# this will be resolved so can avoid converting spatRaster to raster.

## Setup ----

#### * Dates 
# Used in map titles
# Current dates and year
#current_date <- Sys.Date()
#current_year <- as.numeric(format(current_date, format = "%Y"))
current_year <- 2021
current_date <- as.Date(paste0("Jun-11-", current_year), format = "%b-%d-%Y") 
last_year <- current_year - 1
lastYr_date <- as.Date(gsub(current_year, last_year, current_date))

#### * Mobile detect

#### * Spatial features ####
# Spatial features to add to map
# State boundaries
state_sf <- tigris::states() %>% 
  filter(STATEFP %in% c(41,53)) %>%
  st_transform(crs = 4326)

# County boundaries
county_sf <- tigris::counties(state = c(41,53)) %>%
  st_transform(crs = 4326)

# Functions ----

# Function that removes leading 0s in map title dates
DateFormat <- function(dat) {
  str_glue("{month(dat)}/{day(dat)}/{year(dat)}")
}

# Function to import outputs (rasters)
# Raster with total cumulative DDs has multiple layers so need only last layer, 
# which corresponds to cumulative DDs on last sampling day (= 4 days from current date)
# Rasters with 3- and 4-day risk have only 1 layer
RastImport <- function(file_name) {
  rast_stack <- rast(file_name)
  # Round up to nearest 0.5 
  rast <- ceiling(rast_stack[[nlyr(rast_stack)]] / 0.5) * 0.5 
  crs(rast) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  return(rast)
}

# Function to factorize a raster (translate numbers to a category)
FactorizeRast <- function(r, type) {
  
  # 3- and 4-day risk - four levels
  if (grepl("Day", type)) {
    # Values have to be rounded to factorize
    r <- round(r)
    # Unique values up to 4 ("High risk" is always >= 4)
    vals <- unique(values(r, na.rm = TRUE))
    vals <- vals[vals <= 4]
    # Levels
    lvls <- data.frame(id = vals) %>%
      mutate(
        risk = ifelse(id < 1, "Very low", 
                      ifelse(id >= 1 & id < 2, "Low/Medium", 
                             ifelse(id >= 2 & id < 3, "Medium", 
                                    ifelse(id >= 3 & id < 4, "Medium/High", "High"))))) %>%
      arrange(id)
    # Factorize raster
    levels(r) <- lvls
    
    # Cumulative risk is only other option - cut values into bins of 5
  } else {
    # Values have to be rounded to factorize
    r <- round(r)
    # Unique raster values
    vals <-  sort(unique(values(r, na.rm = TRUE)))
    lvls <- data.frame(id = vals, risk = vals) %>%
      mutate(risk = case_when(risk <= 5 ~ "0-5", 
                               risk > 5 & risk <= 10 ~ "6-10", 
                               risk > 10 & risk <= 15 ~ "11-15", 
                               risk > 15 & risk <= 20 ~ "16-20",
                               risk > 20 & risk <= 25 ~ "21-25",
                               risk > 25 & risk <= 30 ~ "26-30",
                               risk > 30 & risk <= 35 ~ "31-35",
                               risk > 35 & risk <= 40 ~ "36-40",
                               risk > 40 & risk <= 45 ~ "41-45",
                               risk > 45 & risk <= 50 ~ "46-50",
                               risk > 50 ~ ">50"))
    # Factorize raster
    levels(r) <- lvls
  }

  return(r)
}

# Produce a leaflet map showing risk of infection
RiskMap <- function(input, raster_current, raster_lastYr, title_current, title_lastYr, 
                    lgd_title, pal_risk_current, pal_risk_lastYr, unique_vals_current, 
                    unique_vals_lastYr, map_width, address_submit, coords) {
  
  
  # Using a height of 1000 and minZoom = 7 is another option but font too
  # small and can't figure out an easy way to adjust font size.
  #### * Current year map ####
  map_current <- leaflet(height = 500, #width = map_width(),
                         options = leafletOptions(
                           zoomControl = FALSE, minZoom = 6)) %>% 
    
    # Add OpenStreetMap layer
    addProviderTiles(providers$CartoDB.Voyager) %>%
    
    # Risk layer output
    addRasterImage(raster_current, color = pal_risk_current, opacity = 0.5,
                   group = "Risk value", layerId = "Risk value") %>%
    
    # Risk layer raster query (use project = TRUE or get wrong values)
    addImageQuery(raster(raster_current), project = TRUE, prefix = "", digits = 2,
                  layerId = "Risk value", position = "topleft", 
                  type = "mousemove") %>%
    
    # Add county lines / markers
    addPolylines(data = state_sf, group = "States", opacity = 0.25, 
                 color = "black", weight = 1.75) %>%
    addPolylines(data = county_sf, group = "Counties", opacity = 0.15, 
                 color = "black", weight = 1) %>%
    
    ###  Other leaflet map features
    # Shows map coordinates as mouse is moved over map
    addMouseCoordinates %>%
    
    fitBounds(lng1 = -127, lat1 = 41.98, lng2 = -120.5, lat2 = 49.1664) %>%
    
    # Change position of zoom control buttons
    htmlwidgets::onRender("function(el, x) {
      L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>%
    
    # Map title
    addControl(title_current, position = "bottomright", className = "map-title")
  
  #### * Last year map ####
  # Leaflet map for same date last year
  map_lastYr <- leaflet(height = 500, width = map_width(),
                        options = leafletOptions(zoomControl = FALSE, minZoom = 6)) %>% 
    
    # Add OpenStreetMap layer
    addProviderTiles(providers$CartoDB.Voyager) %>%
    
    # Risk layer output
    addRasterImage(raster_lastYr, color = pal_risk_lastYr, opacity = 0.5,
                   group = "Risk value (last year)", 
                   layerId = "Risk value (last year)") %>%
    
    # Risk layer raster query (use project = TRUE or get wrong values)
    addImageQuery(raster(raster_lastYr), project = TRUE, prefix = "", digits = 2,
                  layerId = "Risk value (last year)", position = "topleft",
                  type = "mousemove") %>%
    
    # Add county lines / markers
    addPolylines(data = state_sf, group = "States", opacity = 0.25, 
                 color = "black", weight = 1.75) %>%
    addPolylines(data = county_sf, group = "Counties", opacity = 0.15, 
                 color = "black", weight = 1) %>%
    
    ###  Other map features
    # Shows map coordinates as mouse is moved over map
    addMouseCoordinates %>%
    
    # Set bounds
    fitBounds(lng1 = -127, lat1 = 41.98, lng2 = -120.5, lat2 = 49.1664) %>%
    
    # Change position of zoom control buttons
    htmlwidgets::onRender("function(el, x) {
      L.control.zoom({ position: 'topright' }).addTo(this)
    }")  %>%
    
    # Map title
    addControl(title_lastYr, position = "bottomright", className = "map-title")
  
  #### * Final map add-ons ####
  ## Add legend
  # Legend for current year map
  lgd_vals_current <- factor(unique_vals_current, 
                             levels = unique(levels(raster_current)[[1]]$risk))
  map_current <- map_current %>%
    addLegendFactor(title = lgd_title, 
                    pal = colorFactor(pal_risk_current, lgd_vals_current),
                    values =  lgd_vals_current, position = "bottomleft",
                    width = 10, height = 10, labelStyle = 'font-size: 12px;')
  
  # Legend for last year map
  lgd_vals_lastYr <- factor(unique_vals_lastYr, 
                            levels = unique(levels(raster_lastYr)[[1]]$risk))
  map_lastYr <- map_lastYr %>%
    addLegendFactor(title = lgd_title, 
                    pal = colorFactor(pal_risk_lastYr, lgd_vals_lastYr),
                    values =  lgd_vals_lastYr,  position = "bottomleft",
                    width = 10, height = 10, labelStyle = 'font-size: 12px;')
  
  # Below statement is only for risk maps that have an address query
  # If input address doesn't return NULL coordinates, add markers and zoom
  if (address_submit == 1) {
    
    if (!(is.na(coords()$lat)) & !(is.na(coords()$long))) {
      
      # Current year
      map_current <- map_current %>%
        addCircleMarkers(lat = coords()$lat, lng = coords()$long,
                         opacity = 0.75, color = "cyan", 
                         weight = 3, fill = FALSE) %>%
        setView(lng = coords()$long, lat = coords()$lat, zoom = 11)
      
      # Last year
      map_lastYr <- map_lastYr %>%
        addCircleMarkers(lat = coords()$lat, lng = coords()$long,
                         opacity = 0.75, color = "cyan", 
                         weight = 3, fill = FALSE) %>%
        setView(lng = coords()$long, lat = coords()$lat, zoom = 11)
      
    } 
    
  }
  
  # Return a synced "current date" and "last year same date" map 
  # if desired, otherwise just return map for current date
  if (input$lastYr_checkbox ==  1) {
    out_map <- sync(map_current, map_lastYr, sync.cursor = FALSE)
  } else {
    out_map <- map_current
  }
  
}

# Import and process model outputs ----

# File names
fls <- c("Cum_Inf_Risk_Total.tif", "Cum_Inf_Risk_3day.tif", "Cum_Inf_Risk_4day.tif")
outdir_current <- paste0("https://github.com/bbarker505/BOXB-webapp/raw/main/rasters/ref_6-11/", current_year)
outdir_lastYr <- paste0("https://github.com/bbarker505/BOXB-webapp/raw/main/rasters/ref_6-11/", last_year)

# Model outputs for current run 
rasts_current <- map(
  fls, function(file_name) {
    RastImport(paste0(outdir_current, "/", file_name))
  }
)

# Model outputs for same time last year
rasts_lastYr <- map(
  fls, function(file_name) {
    RastImport(paste0(outdir_lastYr, "/", file_name))
  }
)

# Custom map title CSS specs ----
# border-radius makes rounded edges
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    padding-left: 3px; 
    padding-right: 3px; 
    padding-top: 2px; 
    padding-bottom: 2px;
    border-radius: 2px;
    background: rgba(255,255,255,.75);
    font-size: 14px;
    font-weight: bold;
    text-align: center;
    color: black;
  }
"))

# User interface ------

#### * Color themes ####
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_global(
    content_bg = "#FAFCFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)

#### * Fluid page layout ####
ui <- fluidPage(
  
  tags$style(".container-fluid {
             background-color: #d4ddc0;
             }"),
  
  #  Title and windowTitle (this is not a dashboard header, which has a dropdown menu)
  titlePanel(div("Boxwood Blight Risk Mapping Tool for Western OR and WA",
                 style = "color: black; 
                 font-size: 28px; 
                 font-weight: bold;
                 margin-left: 15px;
                 font-family: 'Source Sans Pro', 'Helvetica Neue', Helvetica, Arial, sans-serif;"),
             windowTitle = "Boxwood Blight Risk Mapping"),
  
  #shinybrowser::detect(),
  
  dashboardPage(
    
    # Dashboard header disabled (used titlePanel instead)
    dashboardHeader(disable = TRUE),
    
    # No dashboard sidebar 
    dashboardSidebar(minified = FALSE, disable = TRUE, collapsed = TRUE),
    
    # Main body of dashboard
    dashboardBody(
      
      use_theme(mytheme),
      
      tags$head(tags$style(HTML(".popover-title{ font-weight: bold;}"))),
      
      # Background info 
      fluidRow(
        style = "font-size:19px;",
        box(title = strong("Overview", style = "font-size:22px"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            color = "light-blue",
            fluidRow(
              style = "font-size:19px;",
               column(width = 10, 
                     offset = 0, 
                     p("The boxwood blight infection risk mapping tool uses gridded climate data to calculate the risk of boxwood being infected by boxwood blight in western Oregon and Washington. Three day and four day forecasts of infection risk are available, as well as a map of cumulative risk (Jan 1 to date of four day forecast). Climate data are derived from the ", a(href = "https://www.prism.oregonstate.edu", "PRISM", target = "_blank"), "database at a 800 m", tags$sup(2, .noWS = "before"), " resolution. Presently models are run only for areas west of the Cascades (approximately west of \u2013120.5\u00B0W). Please see a", a(href = "https://www.prism.oregonstate.edu", "tutorial", target = "_blank"), "for details on tool use and map interpretation. Expand the Introduction below to learn more about boxwood blight and risk models for this disease."))))),
      
      # Background info 
      fluidRow(
        style = "font-size:19px;",
        box(title = strong("Introduction", style = "font-size:22px"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            color = "light-blue",
            fluidRow(
              style = "font-size:19px;",
              column(width = 2, offset = 0, align = "center", style='padding:0px;font-size:14px;',
                     img(src = "https://raw.githubusercontent.com/bbarker505/boxb-ddrp/main/Images/boxb-infected-shrubs.png", width = "160px"),
                     img(src = "https://raw.githubusercontent.com/bbarker505/boxb-ddrp/main/Images/boxb-infected-leaves.png", width = "160px" ),
                     img(src = "https://raw.githubusercontent.com/bbarker505/boxb-ddrp/main/Images/boxb-infected-stems.png", width = "160px")),
              column(width = 10, offset = 0, 
                       p(strong("Background: "), "Boxwood blight caused by the fungus ", em("Calonectria pseudonaviculata"), " can result in defoliation, decline, and death of susceptible varieties of boxwood, including most varieties of ", em("Buxus sempervirens"), " such as \u0022Suffruticosa\u0022  (English boxwood) and \u0022Justin Brouwers\u0022. Images show diagnostic symptoms of boxwood blight including defoliation ", strong("(top)"), ", leaf spots ", strong("(middle)"), " and black streaks on stems ", strong("(bottom)"), "(courtesy of Chuan Hong). The fungus has been detected at several locations (mostly in nurseries) in at least six different counties in Oregon and is thought to be established in some areas. Previous", a(href = "https://doi.org/10.3390/biology11060849", "research", target = "_blank"), "indicates that western Oregon and Washington have highly suitable climates for establishment of", em("C. pseudonaviculata"),  ". Tools are therefore needed to inform growers and gardeners about when environmental conditions are conducive to boxwood blight infection and establishment."),
                     p("Generally, it should be very humid or raining and at moderately warm temperatures (60-85 \260F) for a couple days for boxwood blight infection risk to be high. An inoculum source must be present nearby for infection to occur. Overhead irrigation facilitates outbreaks because it creates higher relative humidity and exposes leaf surfaces to longer periods of leaf wetness. For more information on preventing and managing boxwood blight, see the ", a(href = " https://pnwhandbooks.org/plantdisease/host-disease/boxwood-buxus-spp-boxwood-blight", "Pacific Northwest Pest Management Handbook", target = "_blank"), " and a ", a(href = " https://www.pubs.ext.vt.edu/content/dam/pubs_ext_vt_edu/PPWS/PPWS-29/PPWS-29-pdf.pdf", "publication", target = "_blank"),"by Virginia Cooperative Extension."),
                     p(strong("Tool description: "), "The risk mapping tool differs from the ", a(href = "https://uspest.org/risk/boxwood_app", "boxwood blight model app"), "and the", a(href = "https://uspest.org/risk/boxwood_map", "synoptic map-view of risk"), " available at", a(href = "https://uspest.org", "USPest.org"), "because it uses daily gridded climate data instead of hourly climate data from single weather stations. Spatialized predictions allow you to visualize risk for all areas in western Oregon and Washington, from a single neighborhood to an entire city or region. The spatial model is run using a modified version of a platform known as ", a(href = "https://uspest.org/CAPS/", "DDRP"), ", which provides real-time forecasts of phenology and establishment risk of 16 species of invasive insects in the contiguous US. It will likely need to fine-tuned as more infection incidence data become available."),
                     p(strong("Disclaimer: "), "The index is intended to inform your decisions about management actions, such as choice and timing of control measures and intensity of scouting. It should supplement, not replace, the other factors you consider in making these decisions. Use at your own risk."))))),
      
      # Risk map menu
      fluidRow(
        style = "font-size:19px;",
        box(title = strong("Risk Map", style = "font-size:22px"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = 12,
          color = "light-blue",
          ## Risk map selection
          fluidRow(style = "font-size:19px;",
                   column(width = 1),
                   column(
                     width = 3,
                     offset = 0,
                     radioButtons("risk",
                       label = tags$span("Select risk map", 
                                         bsButton("info_maptype", 
                                                  label = "", 
                                                  icon = icon("info"), 
                                                  style = "info", 
                                                  size = "extra-small")),
                       choices = c("Three Day", "Four Day", "Cumulative (Total)"),
                       selected = "Three Day"),
                # Popover info box
                shinyBS::bsPopover(
                  id = "info_maptype",
                  title = "Information",
                  content = paste0("Three Day and Four Day risk maps show forecasts of infection risk for ", DateFormat(current_date + 3), " and ",  DateFormat(current_date + 4), ", respectively. The Cumulative (Total) risk map shows predictions of total risk accumulation between the start of the year and ", DateFormat(current_date + 4), ". Hover the mouse over a location on the map to see its risk value."),
                  placement = "right",
                  trigger = "hover",
                  options = list(container = "body"))),
              ## Address selection and entry
              column(
                width = 3,
                offset = 0,
                checkboxInput("address_checkbox",
                              value = FALSE,
                              label = tags$span("Locate an address",
                                                bsButton("info_address",
                                                         label = "",
                                                         icon = icon("info"),
                                                         style = "info",
                                                         size = "extra-small"))),
                
                # Popover info box
                shinyBS::bsPopover(
                  id = "info_address",
                  title = "Information",
                  content = "Marks, zooms to, and extracts risk information for a specified location. After checking the box, enter a city name, address, or place in the white box and press Submit.",
                  placement = "right",
                  trigger = "hover",
                  options = list(container = "body")),
                # Conditional panel of address box is selected
                conditionalPanel(
                  condition = "input.address_checkbox == 1",
                  textInput("address", "Enter an address, city, or place",
                            value = ""),
                  actionButton("address_submit", "Submit"))),
              column(
                width = 3, 
                checkboxInput("lastYr_checkbox",
                              value = FALSE,
                              label = tags$span("Compare to last year",
                                                bsButton("info_lastYr",
                                                         label = "",
                                                         icon = icon("info"),
                                                         style = "info",
                                                         size = "extra-small"))),
                shinyBS::bsPopover(
                  id = "info_lastYr",
                  title = "Information",
                  content = "Produces a second map that shows risk for the same time last year. This may provide insight into how climate differences between years affects infection risk. The two risk maps are synced, so panning and zooming one map will do the same for the other.",
                  placement = "right",
                  trigger = "hover",
                  options = list(container = "body"))),
              )),
        
        # Error message
        #uiOutput("address_error"),
        fluidRow(style="padding-left:15px;margin-top:1em;font-size:19px;",
                 column(width = 12, uiOutput("address_error")),
                 column(width = 12, uiOutput("coords_error"))),
        
        # Risk map value for geocoded address
        conditionalPanel(
          condition = "input.address_checkbox == 1",
          fluidRow(style = "font-size:19px;padding-left:15px;",
                   column(width = 6, uiOutput("coords_value_current")),
                   column(width = 6, uiOutput("coords_value_lastYr")))),
        
        # Risk maps
        fluidRow(style = "padding-left:15px;",
                 column(width = 12, uiOutput("risk_map"))),
        
        # Acknowledgements
        fluidRow(style="padding-left:15px;margin-top:1.5em;font-size:19px;",
                 box(title = strong("Acknowledgements", style = "font-size:22px"),
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = FALSE,
                     width = 12,
                     color = "light-blue",
                     fluidRow(
                       style = "font-size:19px;",
                       column(width = 12, 
                              p("Creation of this app was funded by an Oregon Department of Agriculture Nursery Research Grant number ODA-4310-A. Boxwood blight risk modeling work was funded by the US Farm Bill FY17 and sponsored by the USDA APHIS PPQ agreement number 20-8130-0282-CA, and by National Institute of Food and Agriculture Crop Protection and Pest Management Extension Implementation Program grant number 2021-70006-35581. Many collaborators from OSU, USDA ARS, Virginia Tech, and North Carolina State University helped improve the model."))))),
      
      # Logos
      fluidRow(
        column(width = 3, align = "center", offset = 0,
               img(src = "https://raw.githubusercontent.com/bbarker505/boxb-ddrp/main/Images/OIPMC.png", width = "75%")),
        column(width = 3, align = "center", offset = 0,
               img(src = "https://raw.githubusercontent.com/bbarker505/boxb-ddrp/main/Images/Oregon-Department-of-Agriculture-logo.png", width = "75%")),
        column(width = 3, align = "center", offset = 0,
               img(src = "https://raw.githubusercontent.com/bbarker505/boxb-ddrp/main/Images/PRISM.png", width = "65%")),
        column(width = 3, align = "center", offset = 0,
               img(src = "https://raw.githubusercontent.com/bbarker505/boxb-ddrp/main/Images/usda-logo_original.png", width = "45%")))))))

# Server ----
server <- function(input, output) {
  
  # Resize map widths based on whether map for last year is selected
  # 100%: maps (both years) span entire column width (=12)
  # 50%: current map only spans half the column width (=6)
  # Expression used in leaflet maps below
  map_width <- reactive({
    
    if (input$lastYr_checkbox == 1) {
      "100%"
    } else {
      "50%"
    }
  })
  
  #### * Error messages ####
  # Report error message if "submit" button is hit before typing an address
  address_error <- eventReactive(input$address_submit, {
    validate(
      need(input$address != "", "Please enter an address or location")
    )
  })
  output$address_error <- renderUI({ # Send output to UI
    address_error()
  })
  
  # Report error message if coordinates cannot be obtained for an address
  coords <- eventReactive(input$address_submit, {
    if (input$address != "") { 
      tribble(~addr, input$address) %>%
        geocode(addr)  
    }
  })
  
  # NA values will be in coordinates output if address couldn't be geocoded
  output$coords_error <- renderText({
    if (any(is.na(coords()))) # If any NA values, return message
      return("<font color=\"#7F7F7F\">Sorry, this address could not
             be geocoded! Check for typos.</font>")
  })
  
  # Generate leaflet maps ----
  # Different maps are rendered depending on radioButton inputs
  observeEvent(input$risk, {
    
    #### * Rasters to map ####
    # Rasters for this year
    raster_current <- switch(input$risk,
                             "Three Day" = rasts_current[[2]],
                             "Four Day" = rasts_current[[3]],
                             "Cumulative (Total)" = rasts_current[[1]])
    # Rasters for last year
    raster_lastYr <- switch(input$risk,
                            "Three Day" = rasts_lastYr[[2]],
                            "Four Day" = rasts_lastYr[[3]],
                            "Cumulative (Total)" = rasts_lastYr[[1]])

    #### * Map titles with dates ####
    
    # Dates for current year
    title_current <- switch(
      input$risk, 
      "Three Day" = paste(DateFormat(current_date), " \u2013 ", DateFormat(current_date + 3)),
      "Four Day" = paste(DateFormat(current_date), " \u2013 ", DateFormat(current_date + 4)),
      "Cumulative (Total)" = paste0("1/1/", current_year, " \u2013 ", DateFormat(current_date + 4)))
    title_current <- tags$div(tag.map.title, HTML(title_current))
    
    # Dates for last year
    title_lastYr <- switch(
      input$risk, 
      "Three Day" = paste(DateFormat(lastYr_date), " \u2013 ", DateFormat(lastYr_date + 3)),
      "Four Day" = paste(DateFormat(lastYr_date), " \u2013 ", DateFormat(lastYr_date + 4)),
      "Cumulative (Total)" = paste0("1/1/", last_year, " \u2013 ", DateFormat(lastYr_date + 4)))
    title_lastYr <- tags$div(tag.map.title, HTML(title_lastYr))
    
    #### * Map legend and color palette ####
    # Legend title
    lgd_title <- switch(input$risk,
                        "Cumulative (Total)" = "Total Risk",
                        "Three Day" = "3 Day Risk",
                        "Four Day" = "4 Day Risk")
    
    # Color palettes
    # Long-term risk (total for year) uses a continuous scale
    # Using same scale for both maps requires using scales for the 
    # raster with the.5 highest risk value (i.e. greatest range of values)
    both_rasters <- c(raster_current, raster_lastYr)
    max_rast <- round(app(both_rasters, max))
    
    # Convert rasters to factor to categorical only for 3- and 4-day risk
    # Also define palettes for categorical maps
    # Legend title is used to define the type of risk map (short-term vs. cumulative)
    max_rast <- FactorizeRast(max_rast, lgd_title)
    raster_current <- FactorizeRast(raster_current, lgd_title)
    raster_lastYr <- FactorizeRast(raster_lastYr, lgd_title)
      
    # Need to know number of unique values for color ramp
    #ncols <- nrow(unique(values(max_rast, na.rm = TRUE)))
    ncols <-  length(unique(levels(max_rast)[[1]]$risk))
    
    # Palette depends on risk map type
    # Maximum of 5 colors for 3- and 4-day vs. 11 colors for cumulative
    # Green-yellow-red
    if (grepl("Day", lgd_title)) {
      pal_risk <- colorRampPalette(c("#009405", "#ffff00", "#da9101", "#c30010"))(5)
    } else {
      pal_risk <- colorRampPalette(c("#009405", "#ffff00", "#da9101", "#c30010"))(11)
    }
      
    # Retail needed levels only
    pal_risk <- pal_risk[1:ncols]
    
    # Define attributes for rasters so that legend shows values correctly
    unique_vals_current <- unique(levels(raster_current)[[1]]$risk)
    unique_vals_lastYr <- unique(levels(raster_lastYr)[[1]]$risk)
    pal_risk_current <- pal_risk[1:length(unique_vals_current)]
    pal_risk_lastYr <- pal_risk[1:length(unique_vals_lastYr)]
    
    # Render maps ----
    
    # Produce leaflet map using RiskMap function
    # This map is used only if an address hasn't been submitted
    output$risk_map <- renderUI({ 
      RiskMap(input, raster_current, raster_lastYr, title_current, title_lastYr, 
              lgd_title, pal_risk_current, pal_risk_lastYr, unique_vals_current, 
              unique_vals_lastYr, map_width, address_submit = 0, coords = NA) 
      })
    
    # Below updates maps each time a new address (location) is submitted
    # Not sure else how to do this besides having a nested "observeEvent"
    observeEvent(input$address_submit, {
      
      address_submit <- 1
      
      # Produce leaflet map using RiskMap function
      # Coordinates of address are passed into the function this time
      output$risk_map <- renderUI({ 
        RiskMap(input, raster_current, raster_lastYr, title_current, title_lastYr, 
                lgd_title, pal_risk_current, pal_risk_lastYr, unique_vals_current, unique_vals_lastYr,
                map_width, address_submit = 1, coords = coords) 
      })
        
        # Add circle markers for geocoded location if coordinates are not NA
        output$coords_value_current <- renderText({
          
          if (input$address != "") {
            
            xy <- data.frame(x = coords()$long, y = coords()$lat)
            rast_val <- terra::extract(raster_current, xy)[1,2]
            
            if (!is.na(rast_val)) {
              paste(input$risk, "Risk at Location:", rast_val)
            } else {
              "No risk forecast for this location"
            }
            
          }
        })
        
        # Last year risk map
        output$coords_value_lastYr <- renderText({
          
          if(input$address != "" & input$lastYr_checkbox == 1) {
            
            xy <- data.frame(x = coords()$long, y = coords()$lat)
            rast_val <- terra::extract(raster_lastYr, xy)[1,2]
            
            if (!is.na(rast_val)) {
              
              paste(input$risk, "Risk at Location:", rast_val)
              
            } else {
              "No risk forecast for this location - please try again"
            }
            
          }
          
        })
        
      })
      
    })

}

# Run app ----
shinyApp(ui = ui, server = server)

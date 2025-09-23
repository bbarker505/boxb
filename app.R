##### Boxwood blight risk mapping app for western Oregon and Washington #####
# Purpose: display outputs forecasts of 3-day, 4-day, and cumulative  risk 
# Designed to run from day 1 of year through 4 days past current date
# For example, if today is 8/1/23, then runs for 1/1/23 to 8/5/23

options(shiny.sanitize.errors = FALSE)

# Packages
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
library(lubridate) # Working with dates
library(tidygeocoder) # Obtain coordinates from address
library(shiny) # Web app 
library(shinyWidgets)
library(shinydashboard)
library(shinyBS) # Info tabs next to risk map menu items
library(shinycssloaders) # "Loading" animation for risk maps (waiting)
library(shinyjs) # For "delay" function to causes error messages to disappear
library(bslib)
library(fresh) # Color theme for web app page
library(htmlwidgets)

# Last update - 11/2/23 
# Updated code so that last year map wouldn't zoom if coords fell outside W OR/WA

## Setup ----

Sys.setenv(MAPQUEST_API_KEY = "5vjLXIpEjMHpANFr4Ok2BNxpuQPrsGQP")

#### * Dates 
# Used in map titles
# Current dates and year
current_date <- Sys.Date()
current_year <- as.numeric(format(current_date, format = "%Y"))
#current_year <- 2021
#current_date <- as.Date(paste0("Jun-11-", current_year), format = "%b-%d-%Y") 
last_year <- current_year - 1

# Must deal with leap day or app will crash
# Get data for March 1 for last year if today is a leap day
if (current_year %% 4 == 0) {
#if (current_year == as.Date(paste0(current_year, "-02-29"))) {
  lastYr_date <- as.Date(gsub(current_year, last_year, current_date + 1))
} else {
  lastYr_date <- as.Date(gsub(current_year, last_year, current_date))
}

# Spatial features to add to map (all have CRS = WGS 84)
# State boundaries
state_sf <- st_read("./features/states_OR_WA.shp")

# County boundaries
county_sf <- st_read("./features/counties_OR_WA.shp") 

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
  
  # Must recode raster because raster factorization doesn't work when 
  # there are "duplicate" values (e.g., if 1.5 and 2 are both "High Risk"),
  # and apparently it won't accept decimal values? This is strange.
  r[r>=2] <- 4 # Values >2 are 5-8 lesions
  r[r==1.5] <- 3 # Values of 1.5 are 1-6 lesions
  r[r==1] <- 2 # Values of 1 are 1st infection susc. varieties
  r[r==0.5] <- 1 # Values of 0.5 are low risk 
  
  # Unique values up to 4 ("High risk" is always >= 4)
  vals <- unique(values(r))
  vals <- vals[!is.na(vals)]
  
  #Levels
  lvls <- data.frame(ID = vals) %>%
    mutate(
      risk = case_when(ID == 0 ~ "0: Very Low Risk",
                       ID == 1 ~ "1: Low Risk",
                       ID == 2 ~ "2: 1st Infec. Susc. Vars.",
                       ID == 3 ~ "3: Up to 1-6 Lesions",
                       ID == 4 ~ "4: Up to 5-18 Lesions")) %>%
    arrange(ID)
  
  # Factorize raster
  levels(r) <- lvls
  
  return(r)
}

# Produce a leaflet map showing risk of infection
RiskMap <- function(input, rast, pal, map_title, lgd_title, unique_vals, last_year) {
  
  # Need different layer IDs (for "addImageQuery") and zoom/drag options 
  # Last year map
  if (last_year == 1) {
    layerID <- "Value (last year)"
    map <- leaflet(#height = 500, 
      options = leafletOptions(
        attributionControl = FALSE, zoomControl = FALSE, dragging = FALSE,
        doubleClickZoom = FALSE, touchZoom = FALSE, boxZoom = FALSE, 
        scrollWheelZoom = FALSE, minZoom = 6))
    
    # Current year map
  } else {
    layerID <- "Value"
    map <- leaflet(#height = 500, 
      options = leafletOptions(
        attributionControl = FALSE, zoomControl = FALSE, minZoom = 6)) %>% 
      
      # Change position of zoom control buttons
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
      }")
    
  }
  
  # Add additional map features
  map <- map %>%
    # Add OpenStreetMap layer
    addProviderTiles(providers$CartoDB.Voyager)  %>%
    #addProviderTiles(providers$Stamen.TonerLite)  %>%
    # Risk layer output
    addRasterImage(rast, color = pal, opacity = 0.65,
                   group = layerID, layerId = layerID) %>%
    # Risk layer raster query (use project = TRUE or get wrong values)
    # Changed from "mousemove" to "mousemove" because value would sometimes get "stuck" (wouldn't update)
    addImageQuery(raster(rast), project = TRUE, prefix = "", digits = 0,
                  layerId = layerID, position = "topleft", type = "mousemove") %>%
    # Add county lines / markers
    addPolylines(data = state_sf, group = "States", opacity = 0.25, 
                 color = "black", weight = 1.75) %>%
    addPolylines(data = county_sf, group = "Counties", opacity = 0.15, 
                 color = "black", weight = 1.25) %>%
    # Max bounds prevents zooming out past western OR and WA
    setMaxBounds(lng1 = -127, lat1 = 41.966, lng2 = -120.5, lat2 = 49.1664) %>%
    # Map title
    addControl(map_title, position = "bottomleft", className = "map-title") %>%
    #addControl(map_title, className = "map-title") %>%
    # Shows map coordinates as mouse is moved over map
    addMouseCoordinates
  
  # TO DO: Could not figure out how to put legend outside of map!!!
  # The legend gets in the way when viewing the app on a phone
  ## Final map features
  ## Add legend (only to current year map)
  ## TO DO: figure out how to make legend background fully opaque
  # lgd_vals <- factor(unique_vals, levels = unique(levels(rast)[[1]]$risk))
  # map <- map %>%
  #   addLegendFactor(title = lgd_title, 
  #                   pal = colorFactor(pal, lgd_vals),
  #                   values =  lgd_vals, 
  #                   orientation = "horizontal",
  #                   width = 10, height = 10, 
  #                   labelStyle = 'font-size: 14px;')
  return(map)
}

# Import and process model outputs ----

# File names
fls <- c("Cum_Inf_Risk_1day.tif", "Cum_Inf_Risk_2day.tif","Cum_Inf_Risk_3day.tif", "Cum_Inf_Risk_4day.tif")
outdir_current <- paste0("./rasters/today_maps/Misc_output")
outdir_lastYr <- paste0("./rasters/today_lastYr_maps/Misc_output/")
#outdir_current <- "/srv/shiny-server/boxb/rasters/today_maps/Misc_output"
#outdir_lastYr <-  "/srv/shiny-server/boxb/rasters/today_lastYr_maps/Misc_output"


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
    width: 130px;
    padding-left: 3px; 
    padding-right: 3px; 
    padding-top: 2px; 
    padding-bottom: 2px;
    border-radius: 2px;
    background: rgba(255,255,255,.75);
    font-size: 16px;
    font-weight: bold;
    text-align: left;
    color: rgb(51, 51, 51);
  }
"))

# TO DO: Figure out way to control leaflet legend background opacity
# border-radius makes rounded edges
# tag.map.legend <- tags$style(HTML("
#   .leaflet-control.legend { 
#     background: rgba(255,255,255,.75);
#   }
# "))

# UI: User interface ------

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
  
  useShinyjs(), # For making error message disappear with "delay"
  
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
            collapsible = TRUE,
            collapsed = FALSE,
            width = 12,
            color = "light-blue",
            fluidRow(
              style = "font-size:19px;",
              column(width = 12, 
                     offset = 0, 
                     p("The boxwood blight infection risk mapping tool produces forecasts of the risk of boxwood being infected by boxwood blight in western Oregon and Washington. This information may help with planning scouting activities and with efforts to prevent or mitigate infections (e.g., with fungicide treatments). Forecasts are available for each day between tomorrow and four days from today. Climate data are derived from the", a(href = "https://www.prism.oregonstate.edu", "PRISM", target = "_blank", style="text-decoration-line: underline;"), "database at a 800 m", tags$sup(2, .noWS = "before"), " resolution and from the", a(href = "https://vlab.noaa.gov/web/mdl/ndfd", "NDFD", target = "_blank", style = "text-decoration-line: underline;"), "database (downscaled from a 2.5 km", tags$sup(2, .noWS = "before"), "to an 800 m", tags$sup(2, .noWS = "before"), "resolution). Presently models are run only for areas west of the Cascades (approximately west of \u2013120.5\u00B0W). Please see a", a(href = "BOXB_webapp_tutorial.pdf", "tutorial", target = "_blank", style="text-decoration-line: underline;"), "for details on tool use and map interpretation. Expand the Introduction below to learn more about boxwood blight and risk models for this disease."))))),
      
      # Background info 
      fluidRow(
        style = "font-size:19px;",
        box(title = strong("About", style = "font-size:22px"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            color = "light-blue",
            fluidRow(
              style = "font-size:19px;",
              column(width = 2, align = "center", style='padding:0px;font-size:14px;',
                     img(src = "boxb-infected-shrubs2.png", width = "155px", style = "max-height: 240px;"),
                     img(src = "boxb-infected-leaves2.png", width = "155px", style = "max-height: 240px;"),
                     img(src = "boxb-infected-stems2.png", width = "160px", style = "max-height: 240px;")),
              column(width = 10, offset = 0, 
                     p(strong("Introduction: "), "Boxwood blight caused by the fungus ", em("Calonectria pseudonaviculata"), " can result in defoliation, decline, and death of susceptible varieties of boxwood, including most varieties of ", em("Buxus sempervirens"), " such as \u0022Suffruticosa\u0022  (English boxwood) and \u0022Justin Brouwers\u0022. Images show diagnostic symptoms of boxwood blight including", strong("(A)"),  "defoliation,", strong("(B)"), "leaf spots, and", strong("(C)"), "black streaks on stems (courtesy of Chuan Hong). The fungus has been detected at several locations (mostly in nurseries) in at least six different counties in Oregon and is thought to be established in some areas. Previous", a(href = "https://doi.org/10.3390/biology11060849", "research", target = "_blank", style="text-decoration-line: underline;"), "indicates that western Oregon and Washington have highly suitable climates for establishment of", em("C. pseudonaviculata"),  ". Tools are therefore needed to inform growers and gardeners about when environmental conditions are conducive to boxwood blight infection and establishment."),
                     p("Generally, it should be very humid or raining and at moderately warm temperatures (60\u201385\u00B0F) for a couple days for boxwood blight infection risk to be high. An inoculum source must be present nearby for infection to occur. Overhead irrigation facilitates outbreaks because it creates higher relative humidity and exposes leaf surfaces to longer periods of leaf wetness. For more information on preventing and managing boxwood blight, see the ", a(href = " https://pnwhandbooks.org/plantdisease/host-disease/boxwood-buxus-spp-boxwood-blight", "Pacific Northwest Pest Management Handbook", target = "_blank", style="text-decoration-line: underline;"), " and a ", a(href = " https://www.pubs.ext.vt.edu/content/dam/pubs_ext_vt_edu/PPWS/PPWS-29/PPWS-29-pdf.pdf", "publication", target = "_blank", style="text-decoration-line: underline;"),"by Virginia Cooperative Extension."),
                     p(strong("Tool description: "), "The risk mapping tool is similar to the ", a(href = "https://uspest.org/risk/boxwood_app", "boxwood blight model app", target = "_blank", style="text-decoration-line: underline;"), "and the", a(href = "https://uspest.org/risk/boxwood_map", "synoptic map-view of risk", target = "_blank", style="text-decoration-line: underline;"), "available at", a(href = "https://uspest.org", "USPest.org", target = "_blank", style="text-decoration-line: underline;"), "except that it uses daily gridded climate data instead of hourly climate data from single weather stations.  The spatial model is run using a modified version of a platform known as", a(href = "https://uspest.org/CAPS/", "DDRP", target = "_blank", .noWS = "after", style="text-decoration-line: underline;"), ", which provides real-time forecasts of phenology and establishment risk of 16 species of invasive insects in the contiguous US. It will likely need to be fine-tuned as more infection incidence data become available. Technical information on the station-based (hourly) model can be found at", a(href = "https://uspest.org/wea/Boxwood_blight_risk_model_summaryV3.pdf", "USPest.org", target = "_blank", .noWS = "after", style="text-decoration-line: underline;"),"."),
                     p(strong("Suggested citation: "), "Barker, B. S., and L. Coop. 2023. Boxwood blight risk mapping app for western Oregon and Washington. Oregon IPM Center, Oregon State University.", a(href = "https://riskmaps.oregonstate.edu/boxb/", "https://riskmaps.oregonstate.edu/boxb/", .noWS = c("after"), style="text-decoration-line: underline;"), "."),
                     p(strong("Source code and feedback: "), "To view the source code, visit the", a(href = "https://github.com/bbarker505/boxb-webapp", "GitHub repository", target = "_blank", .noWS = c("after"), style="text-decoration-line: underline;"), ". To report bugs or provide feedback, please e-mail Brittany Barker at", a(href = "mailto:brittany.barker@oregonstate.edu", "brittany.barker@oregonstate.edu", .noWS = c("after"), style="text-decoration-line: underline;"), "."),
                     p(strong("Disclaimer: "), "The risk index is intended to inform your decisions about management actions, such as choice and timing of control measures and intensity of scouting. It should supplement, not replace, the other factors you consider in making these decisions. Use at your own risk."))))),
      
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
                                    choices = c("One Day", "Two Day", "Three Day", "Four Day"),
                                    selected = "Three Day"),
                       # Popover info box
                       shinyBS::bsPopover(
                         id = "info_maptype",
                         title = "Select risk map",
                         content = paste0("Risk maps show forecasts of infection risk for ", DateFormat(current_date + 1), ", ", DateFormat(current_date + 2), ", ", DateFormat(current_date + 3), ", and ",  DateFormat(current_date + 4), ", respectively. Possible levels of infection risk are: Very Low Risk, Low Risk, 1st Infection of Susceptible Varieties, Up to 1-6 Lesions, and Up to 5-18 Lesions. Hover the mouse over a location on the map to see a numerical risk value."),
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
                         title = "Locate an address",
                         content = "Marks, zooms to, and extracts risk information for a specified location. After checking the box, enter a city name, address, or place in the text box and press Submit.",
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
                         title = "Compare to last year",
                         content = "Produces a second map that shows risk for the same time last year. Comparing th current year vs. last year map may provide insight into how climate differences between years affects infection risk. The two risk maps are synced, so panning and zooming one map will do the same for the other.",
                         placement = "right",
                         trigger = "hover",
                         options = list(container = "body"))),
            )),
        
        # Risk map value for geocoded address
        fluidRow(style="padding-left:15px;margin-top:1em;font-size:19px",
                 conditionalPanel(
                   condition = "input.address_checkbox == 1",
                   # The "zooming to location" message is not fast enough
                   #column(style ="color:#000000;",
                   #     width = 12, uiOutput("search_message")),
                   column(style ="color:#f56954;",
                          width = 12, uiOutput("error_message")))),
        
        # Risk maps
        # For map heights or the entire region won't be shown (cuts off So. OR)
        tags$style(type = "text/css", "#riskmap1 {height: calc(500px) !important;}"),
        tags$style(type = "text/css", "#riskmap2 {height: calc(500px) !important;}"),
        fluidRow(style = "padding-left:15px;padding-right:15px;",
                 column(width = 6, 
                        leafletOutput("riskmap1") %>% withSpinner(color="#0dc5c1")),
                 conditionalPanel(
                   condition =  "input.lastYr_checkbox == 1", 
                   column(width = 6, leafletOutput("riskmap2") 
                          %>% withSpinner(color="#0dc5c1")))),
        
        
        # Legend title
        fluidRow(style = "margin-bottom:0em;padding-left:15px;padding-top:5px",
                 column(width = 6, style = "font-size:22px;margin-bottom:-0.5em;", align = "center", p(strong("Legend"))),
        ),
        
        # Legend
        fluidRow(style = "font-size:19px;margin-top:0em;padding-left:15px;",
                 column(width = 7, align = "align-items:center; justify-content: center;", style = "font-size:16px;padding-left:20px;",
                        column(width = 2, align = "center",  p(strong(div(icon("fa-solid fa-square", style = "background-color:#DCFFE6;color:#656565;")), HTML(paste("0:", "Very Low Risk", sep="<br/>"))))),
                        column(width = 2, align = "center",  p(strong(div(icon("fa-solid fa-square", style = "background-color:#B0FFB0;color:#656565;")), HTML(paste("1:", "Low Risk", sep = "<br/>"))))),
                        column(width = 2, align = "center", p(strong(div(icon("fa-solid fa-square", style = "background-color:#FAFFD0;color:#656565;")), HTML(paste("2:", "1st Infec. Susc. Vars.", sep = "<br/>"))))),
                        column(width = 2, align = "center", p(strong(div(icon("fa-solid fa-square", style = "background-color:#FFE9A6;color:#656565;")), HTML(paste("3:", "Up to 1\u20136 Lesions", sep = "<br/>"))))),
                        column(width = 2, align = "center",  p(strong(div(icon("fa-solid fa-square", style = "background-color:#FFD0E6;color:#656565;")), HTML(paste("4:", "Up to 5\u201318 Lesions", sep = "<br/>")))))
                 )),
        
        # Acknowledgements
        fluidRow(style="padding-left:15px;margin-top:1.5em;margin-right:5px;font-size:19px;",
                 box(title = strong("Acknowledgements", style = "font-size:22px"),
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = TRUE,
                     collapsed = FALSE,
                     width = 12,
                     color = "light-blue",
                     fluidRow(
                       style = "font-size:19px;padding:0px;",
                       column(width = 12, offset = 0, 
                              p("Creation of this app was funded by", a(href = "https://www.oregon.gov/ODA", "Oregon Department of Agriculture", target = "_blank", style="text-decoration-line: underline;"), "Nursery Research Grant no. ODA-4310-A. Boxwood blight risk modeling work was funded by the US Farm Bill FY17,", a(href = "https://www.aphis.usda.gov/aphis/", "USDA APHIS PPQ", target = "_blank", style="text-decoration-line: underline;"), "agreement number 20-8130-0282-CA, and", a(href = "https://www.nifa.usda.gov/grants/programs/crop-protection-pest-management-program", "USDA NIFA CPPM", target = "_blank", style="text-decoration-line: underline;"), "Extension Implementation Program grant no. 2021-70006-35581. Many collaborators from OSU, USDA ARS, Virginia Tech, and North Carolina State University helped improve the model."))))),
        
        # Logos
        fluidRow(
          column(width = 3, align = "center", offset = 0,
                 img(src = "OIPMC.png", width = "75%", style = "max-width: 200px;")),
          column(width = 3, align = "center", offset = 0,
                 img(src = "Oregon-Department-of-Agriculture-logo.png", width = "75%", style = "max-width: 200px;")),
          column(width = 3, align = "center", offset = 0,
                 img(src = "PRISM.png", width = "55%", style = "max-width: 200px;")),
          column(width = 3,
                 align = "center", offset = 0,
                 img(src = "usda-logo_original.png", width = "45%", style = "max-width: 200px;max-height: 100px;")))))))

# Server ----
server <- function(input, output, session) {
  
  
  #### * Import rasters ####
  # Different maps are rendered depending on radioButton inputs
  observeEvent(input$risk, {
    
    # Rasters for this year
    raster_current <- switch(input$risk,
                             "One Day" = rasts_current[[1]],
                             "Two Day" = rasts_current[[2]],
                             "Three Day" = rasts_current[[3]],
                             "Four Day" = rasts_current[[4]])
    # Rasters for last year
    raster_lastYr <- switch(input$risk,
                            "One Day" = rasts_lastYr[[1]],
                            "Two Day" = rasts_lastYr[[2]],
                            "Three Day" = rasts_lastYr[[3]],
                            "Four Day" = rasts_lastYr[[4]])
    
   # FIX ME: why are extents suddenly different (1 extra col in last yr) as of 3/21/2024?
   if (ncol(raster_lastYr) < ncol(raster_current)) {
     raster_current <- raster_current[1:nrow(raster_current),1:507, drop = FALSE]
     raster_current <- crop(raster_current, raster_lastYr)
  }

    #### * Map titles with dates ####
    
    # Dates for current year
    title_current <- switch(
      input$risk, 
      "One Day" = paste0(DateFormat(current_date), " \u2013", DateFormat(current_date + 1)),
      "Two Day" =  paste0(DateFormat(current_date), " \u2013", DateFormat(current_date + 2)),
      "Three Day" = paste0(DateFormat(current_date), " \u2013", DateFormat(current_date + 3)),
      "Four Day" = paste0(DateFormat(current_date), " \u2013", DateFormat(current_date + 4)))
    title_current <- tags$div(tag.map.title, HTML(title_current))
    
    # Dates for last year
    title_lastYr <- switch(
      input$risk, 
      "One Day" = paste0(DateFormat(lastYr_date), " \u2013", DateFormat(lastYr_date + 1)),
      "Two Day" =  paste0(DateFormat(lastYr_date), " \u2013", DateFormat(lastYr_date + 2)),
      "Three Day" = paste0(DateFormat(lastYr_date), " \u2013", DateFormat(lastYr_date + 3)),
      "Four Day" = paste0(DateFormat(lastYr_date), " \u2013", DateFormat(lastYr_date + 4)))
    title_lastYr <- tags$div(tag.map.title, HTML(title_lastYr))
    
    #### * Factorize rasters, define legend and palettes ####
    # Legend title
    lgd_title <- switch(input$risk,
                        "One Day" = "One Day Risk",
                        "Two Day" = "Two Day Risk",
                        "Three Day" = "Three Day Risk",
                        "Four Day" = "Four Day Risk")
    
    # Color palettes
    # Convert rasters to factor 
    # Also define palettes for categorical maps
    # Legend title is used to define the type of risk map (short-term vs. cumulative)
    both_rasters <- c(raster_current, raster_lastYr)
    max_rast <- FactorizeRast(app(both_rasters, max), lgd_title)
    raster_current <- FactorizeRast(raster_current, lgd_title)
    raster_lastYr <- FactorizeRast(raster_lastYr, lgd_title)
    
    # Need to know number of unique values for color ramp
    ncols <-  length(unique(levels(max_rast)[[1]]$risk))
    
    # Palette depends on risk map type
    # Maximum of 5 colors - uses same color ramp as uspest.org
    # Green-yellow-red
    pal_risk <- c("#DCFFE6", "#B0FFB0","#FAFFD0", "#FFE9A6", "#FFD0E6")
    
    # Retain needed levels only
    pal_risk <- pal_risk[1:ncols]
    
    # Define raster attributes so that legend shows values correctly
    unique_vals_current <- unique(levels(raster_current)[[1]]$risk)
    unique_vals_lastYr <- unique(levels(raster_lastYr)[[1]]$risk)
    pal_risk_current <- pal_risk[1:length(unique_vals_current)]
    pal_risk_lastYr <- pal_risk[1:length(unique_vals_lastYr)]
    
    #### * Render Leaflet maps ####
    # Produce and render maps
    # All subsequent modifications of maps below use "LeafletProxy"
    # (this function modifies the map that has already been rendered.
    # Current year map
    output$riskmap1 <- renderLeaflet({ 
      RiskMap(input, raster_current, pal_risk_current, title_current,  
              lgd_title, unique_vals_current, last_year = 0) %>%
        fitBounds(lng1 = -127, lat1 = 41, lng2 = -120.5, lat2 = 49.1664)
    })
    
    # This code block was supposed to ensure that the risk map for last year is in sync
    # (same bounds/zoom) with current year map when map type is switched
    # Otherwise, sometimes the map is zoomed out to entire region until the
    # current map is touched.
    # Doesn't work when checkbox clicked before current map loads.
    # Last year map - overwritten once map bounds from current map is obtained
    output$riskmap2 <- renderLeaflet({ 
      RiskMap(input, raster_lastYr, pal_risk_lastYr, title_lastYr,  
              lgd_title, unique_vals_lastYr, last_year = 1)  
    })
    
    # Re-do map so bounds are same as current map
    observeEvent(input$riskmap1_bounds, {
      
      bounds <- input$riskmap1_bounds
      if (!is.null(bounds)) {
        # Last year map
        output$riskmap2 <- renderLeaflet({ 
          RiskMap(input, raster_lastYr, pal_risk_lastYr, title_lastYr,  
                  lgd_title, unique_vals_lastYr, last_year = 1)  %>%
            fitBounds(bounds$west, bounds$south, bounds$east, bounds$north)
        })
      }
      # Must be TRUE or map will be rendered anytime current map is touched
    }, once = TRUE) 
    
    #### * Bounds: current year map ####
    # Observe bounds of current year map in order to:
    # 1) Keep the bounds from resetting when risk map type changes
    observeEvent(input$riskmap1_bounds, {
      
      # Map zoom can't be entire area (level 6) or get weird behavior
      # (non-stop loop of zooming) when select risk maps multiple times
      bounds <- input$riskmap1_bounds
      mapzoom <- input$riskmap1_zoom
      
      # Keep bounds from resetting
      if (mapzoom > 6) {
        leafletProxy("riskmap1") %>%
          fitBounds(bounds$west, bounds$south, bounds$east, bounds$north)
      }
    })
    
    #### * Maps with location ####
    # Below updates maps each time a new address (location) is submitted
    observeEvent(input$address_submit, {
      
      # Search message
      # Does not appear if coordinates are valid because maps load so quickly 
      # Maybe fix this later
      # output$search_message <- renderText({
      #   "Zooming to location"
      # })
      # delay(2000, output$search_message <- renderText(""))
      
      # Submitted location
      location <- input$address
      # Geocode the location
      coords <- tribble(~addr, location) %>%
        geocode(addr, method = 'mapquest')
      
      # Address submit errors
      output$error_message <- renderText({
        # Error: empty location submission ("")
        if (coords$addr == "") {
          "Please enter a location."
          # Error: a location was entered but could not be geocoded
        } else if (is.na(coords$lat & coords$addr != "")) {
          "Sorry, this location could not be geocoded."
          # Error: a location was valid but falls outside of risk forecast bounds
        } else if (!is.na(coords$lat)) {
          # Determine whether there are predictions for the location
          xy <- data.frame(x = coords$long, y = coords$lat)
          rast_val <- terra::extract(raster_current, xy)[1,2]
          # Error message if rast value is NA
          if (is.na(rast_val)) {
            "No risk forecast for this location."
          }
        }
        
      })
      
      # Make message disappear (requires "shinyjs")
      delay(4000, output$error_message <- renderText(""))
      
      # If input address doesn't return NULL coordinates
      # Add circle markers and zoom to location
      if (!is.na(coords$lat)) {
        
        if (coords$lat > 41.7 & coords$lat < 49.1664 & 
            coords$long > -127 & coords$long < -120.5) {
          
          output$search_message <- renderText({
            "Zooming to location"
          })
          delay(2000, output$search_message <- renderText(""))
          
          # Current year map - can modify rendered map using "leafletProxy"
          # Decided to not show marker for last year map - not necessary
          leafletProxy("riskmap1") %>%
            removeMarker(layerId = "Value") %>% 
            #clearMarkers() %>% # Remove circle markers from last submission
            addRasterImage(raster_current, color = pal_risk_current, opacity = 0.65,
                           group = "Value", layerId = "Value") %>%
            addImageQuery(raster(raster_current), project = TRUE, prefix = "", digits = 0,
                          layerId = "Value", position = "topleft", type = "mousemove") %>%
            addCircleMarkers(lat = coords$lat, lng = coords$long,
                             opacity = 0.75, color = "blue", 
                             weight = 3, layerId = "Value", fill = FALSE) %>%
            setView(lng = coords$long, lat = coords$lat, zoom = 11) # Zooms to area
          
        } 
      }
      
      # If this code chunk is absent, last year map will be zoomed to full extent.
      observeEvent(input$lastYr_checkbox, {
        
        # Make sure map is zoomed to same extent as current year map
        bounds <- input$riskmap1_bounds
        
        if (!is.na(coords$lat)) {
          
          if (coords$lat > 41.700 & coords$lat < 49.1664 & 
              coords$long > -127 & coords$long < -120.5) {
            # Render map
            output$riskmap2 <- renderLeaflet({
              RiskMap(input, raster_lastYr, pal_risk_lastYr, title_lastYr,
                      lgd_title, unique_vals_lastYr, last_year = 1) %>%
                addImageQuery(raster(raster_lastYr), project = TRUE, prefix = "", digits = 0,
                              layerId = "Value (last year)", position = "topleft", type = "mousemove") %>%
                #setView(lng = coords$long, lat = coords$lat, zoom = 11) %>%
                fitBounds(bounds$west, bounds$south, bounds$east, bounds$north)
            })
          }
          
        }
        
      }, once = TRUE) # Don't need to render a new map on each click
      
    })
    
    #### * Maps with no location ####
    # Clears out any error messages and entries from previous submission,
    # and zooms back out to western OR and WA if box is unchecked
    observeEvent(input$address_checkbox, {
      
      # Clear out previous submission text
      if (input$address_checkbox == 0) {
        updateTextInput(session = session, inputId = "address", value = "")
      }
      
      if (input$address_checkbox == 0) {
        # Zoom back out to western OR and WA and clear location markers
        leafletProxy("riskmap1")  %>%
          fitBounds(lng1 = -127, lat1 = 41.7, lng2 = -120.5, lat2 = 49.1664) %>%
          removeMarker(layerId = "Value")  %>% 
          addImageQuery(raster(raster_current), project = TRUE, prefix = "", digits = 0,
                        layerId = "Value", position = "topleft", type = "mousemove") 
        leafletProxy("riskmap2") %>%
          fitBounds(lng1 = -127, lat1 = 41.7, lng2 = -120.5, lat2 = 49.1664) %>%
          removeMarker(layerId = "Value") %>% 
          addImageQuery(raster(raster_lastYr), project = TRUE, prefix = "", digits = 0,
                        layerId = "Value (last year)", position = "topleft", type = "mousemove") 
      }
    })
    
  })
  
  #### * Sync last year map ####
  # Must be outside of "observeEvent' for risk or won't always work correctly
  # Also must use different name for bounds object than above
  observe({
    bounds2 <- input$riskmap1_bounds
    if (!is.null(bounds2)) {
      leafletProxy("riskmap2") %>% 
        fitBounds(bounds2$west, bounds2$south, bounds2$east, bounds2$north)
    }
  })
  
}

# Run app ----
shinyApp(ui = ui, server = server)
